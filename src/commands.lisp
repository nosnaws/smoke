(in-package :smoke)

;;; Command implementations

(defun status ()
  "Show stack status."
  (let* ((branch (current-branch))
         (main (main-branch))
         (commits (stack-commits))
         (state (or (load-state) (init-state branch))))
    (if (null commits)
        (format t "No commits ahead of ~A~%" main)
        (progn
          (format t "~%Stack on ~A (~D commit~:P ahead of ~A):~%~%"
                  branch (length commits) main)
          (loop for commit in commits
                for i from 1
                for patch-id = (getf commit :patch-id)
                for pr = (find-pr-for-patch-id state patch-id)
                do (if pr
                       (let* ((pr-info (ignore-errors (gh-pr-view pr)))
                              (draft (cdr (assoc :is-draft pr-info)))
                              (ci (if pr-info (pr-ci-status pr) :none)))
                         (format t "  ~A ~A  PR #~D ~A ~A~%"
                                 (getf commit :short)
                                 (getf commit :subject)
                                 pr
                                 (if draft "(draft)" "(ready)")
                                 (format-ci-status ci)))
                       (format t "  ~A ~A  (no PR)~%"
                               (getf commit :short)
                               (getf commit :subject))))
          (format t "~%")))))

(defun push-stack ()
  "Create or update PRs for each commit in the stack."
  (ensure-clean-worktree)
  (let* ((branch (current-branch))
         (main (main-branch))
         (commits (stack-commits))
         (state (or (load-state) (init-state branch))))
    (when (null commits)
      (format t "No commits to push.~%")
      (return-from push-stack))

    (format t "Pushing stack of ~D commit~:P...~%~%" (length commits))

    ;; For each commit, create a temporary branch and PR
    (loop for commit in commits
          for i from 1
          for patch-id = (getf commit :patch-id)
          for existing-pr = (find-pr-for-patch-id state patch-id)
          for branch-name = (smoke-branch-name i)
          for base = (if (= i 1) main (smoke-branch-name (1- i)))
          do
             ;; Create/update the branch pointing to this commit
             (create-branch branch-name (getf commit :hash))
             (push-force-with-lease branch-name)

             (if existing-pr
                 ;; Update existing PR's base branch
                 (progn
                   (format t "  ~A ~A  PR #~D (updated)~%"
                           (getf commit :short)
                           (getf commit :subject)
                           existing-pr)
                   (ignore-errors (gh-pr-edit-base existing-pr base)))
                 ;; Create new PR
                 (let* ((is-draft (> i 1))
                        (pr-num (gh-pr-create-simple
                                 (getf commit :subject)
                                 base
                                 branch-name
                                 :draft is-draft)))
                   (format t "  ~A ~A  PR #~D (created~A)~%"
                           (getf commit :short)
                           (getf commit :subject)
                           pr-num
                           (if is-draft ", draft" ""))
                   (setf state (update-state-mapping state patch-id pr-num)))))

    ;; Update state with current branch
    (setf state (list (cons :branch branch)
                      (cons :stack (cdr (assoc :stack state)))))
    (save-state state)
    (format t "~%Done.~%")))

(defun pull-stack ()
  "Rebase onto main, detect merged PRs, update draft states."
  (ensure-clean-worktree)
  (let* ((branch (current-branch))
         (main (main-branch))
         (state (or (load-state) (init-state branch))))

    ;; Fetch latest main
    (format t "Fetching ~A...~%" main)
    (fetch-main)

    ;; Check for merged PRs before rebase
    (let ((stack (cdr (assoc :stack state)))
          (merged-patch-ids nil))
      (loop for entry in stack
            for pr = (cdr (assoc :pr entry))
            for patch-id = (cdr (assoc :patch--id entry))
            when (and pr (pr-merged-p pr))
              do (progn
                   (format t "PR #~D merged.~%" pr)
                   (push patch-id merged-patch-ids)
                   ;; Clean up the smoke branch
                   (let ((branch-name (loop for i from 1
                                            for bn = (smoke-branch-name i)
                                            ;; Find which branch this PR used
                                            thereis (when (ignore-errors
                                                            (let ((pr-info (gh-pr-view pr)))
                                                              (string= (cdr (assoc :head-ref-name pr-info)) bn)))
                                                      bn))))
                     (when branch-name
                       (ignore-errors (delete-remote-branch branch-name))))))

      ;; Remove merged PRs from state
      (when merged-patch-ids
        (setf state (remove-merged-from-state state merged-patch-ids))))

    ;; Rebase onto main
    (format t "Rebasing onto origin/~A...~%" main)
    (rebase-onto (format nil "origin/~A" main))

    ;; Get new commits after rebase
    (let ((commits (stack-commits)))
      (if (null commits)
          (format t "Stack is empty - all PRs merged!~%")
          (progn
            (format t "~%Updating PR draft states...~%")
            ;; Update draft states: only first PR should be ready
            (loop for commit in commits
                  for i from 1
                  for patch-id = (getf commit :patch-id)
                  for pr = (find-pr-for-patch-id state patch-id)
                  when pr
                    do (if (= i 1)
                           (progn
                             (ignore-errors (gh-pr-ready pr))
                             (format t "  PR #~D marked ready~%" pr))
                           (progn
                             (ignore-errors (gh-pr-draft pr))
                             (format t "  PR #~D marked draft~%" pr))))

            ;; Push updated branches
            (format t "~%Pushing updated branches...~%")
            (loop for commit in commits
                  for i from 1
                  for branch-name = (smoke-branch-name i)
                  do (create-branch branch-name (getf commit :hash))
                     (push-force-with-lease branch-name)))))

    (save-state state)
    (format t "~%Done.~%")))

(defun amend-stack ()
  "Interactive amend: pick a commit to amend, then rebase."
  (let ((commits (stack-commits)))
    (when (null commits)
      (format t "No commits in stack to amend.~%")
      (return-from amend-stack))

    (let ((target (interactive-pick-commit commits)))
      (format t "~%Opening editor to amend ~A...~%"
              (getf target :short))
      (format t "After saving, the stack will be rebased.~%~%")
      (amend-commit target))))
