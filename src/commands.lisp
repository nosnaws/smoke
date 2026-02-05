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
                              (pr-st (when pr-info
                                       (cdr (assoc :state pr-info))))
                              (draft (cdr (assoc :is-draft pr-info)))
                              (ci (if pr-info (pr-ci-status-from-info pr-info) :none)))
                         (format t "  ~A ~A  PR #~D ~A ~A~%"
                                 (getf commit :short)
                                 (getf commit :subject)
                                 pr
                                 (cond
                                   ((string= pr-st "CLOSED") "(closed)")
                                   (draft "(draft)")
                                   (t "(ready)"))
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

    ;; For each commit, create a branch and PR (all PRs base on main)
    (loop for commit in commits
          for i from 0
          for patch-id = (getf commit :patch-id)
          for existing-pr = (find-pr-for-patch-id state patch-id)
          for branch-name = (smoke-branch-name patch-id)
          do
             ;; Create/update the branch pointing to this commit
             (create-branch branch-name (getf commit :hash))
             (safe-push-branch branch-name)

             (let ((pr-st (when existing-pr (pr-state existing-pr))))
               (cond
                 ;; PR exists and is open - just update
                 ((eq pr-st :open)
                  (format t "  ~A ~A  PR #~D (updated)~%"
                          (getf commit :short)
                          (getf commit :subject)
                          existing-pr))

                 ;; PR exists but is closed - try to reopen
                 ((eq pr-st :closed)
                  (format t "  ~A ~A  PR #~D (reopening)~%"
                          (getf commit :short)
                          (getf commit :subject)
                          existing-pr)
                  (handler-case
                      (progn
                        (gh-pr-reopen existing-pr)
                        (format t "    Reopened~%"))
                    (error ()
                      (format t "    Could not reopen (may need manual intervention)~%"))))

                 ;; No existing PR - create new
                 (t
                  (let* ((is-draft (> i 0))
                         (pr-num (gh-pr-create-simple
                                  (getf commit :subject)
                                  main  ; Always base on main
                                  branch-name
                                  :draft is-draft)))
                    (format t "  ~A ~A  PR #~D (created~A)~%"
                            (getf commit :short)
                            (getf commit :subject)
                            pr-num
                            (if is-draft ", draft" ""))
                    (setf state (update-state-mapping state patch-id pr-num))
                    (save-state state))))))

    ;; Update state with current branch
    (setf state (list (cons :branch branch)
                      (cons :stack (cdr (assoc :stack state)))))
    (save-state state)
    (format t "~%Done.~%")))

(defun pull-stack ()
  "Rebase onto main, detect merged PRs, reopen closed PRs, update draft states."
  (ensure-clean-worktree)
  (let* ((branch (current-branch))
         (main (main-branch))
         (state (or (load-state) (init-state branch)))
         (closed-prs nil))

    ;; Fetch latest main
    (format t "Fetching ~A...~%" main)
    (fetch-main)

    ;; Check for merged and closed PRs before rebase
    (let ((stack (cdr (assoc :stack state)))
          (merged-patch-ids nil))
      ;; Detect merged and closed PRs
      (loop for entry in stack
            for pr = (cdr (assoc :pr entry))
            for patch-id = (cdr (assoc :patch--id entry))
            when pr
              do (let ((pr-st (pr-state pr)))
                   (cond
                     ((eq pr-st :merged)
                      (format t "PR #~D merged.~%" pr)
                      (push patch-id merged-patch-ids))
                     ((eq pr-st :closed)
                      (format t "PR #~D closed (will reopen).~%" pr)
                      (push (cons patch-id pr) closed-prs)))))

      ;; Remove only merged PRs from state (keep closed ones to reopen)
      (when merged-patch-ids
        (setf state (remove-merged-from-state state merged-patch-ids))))

    ;; Rebase onto main
    (format t "Rebasing onto origin/~A...~%" main)
    (handler-case
        (rebase-onto (format nil "origin/~A" main))
      (rebase-conflict (c)
        (format t "~%~A~%" c)
        (save-state state)
        (return-from pull-stack)))

    ;; Get new commits after rebase
    (let ((commits (stack-commits)))
      (if (null commits)
          (format t "~%Stack is empty - all PRs merged!~%")
          (progn
            ;; Push updated branches first (needed before reopening PRs)
            (format t "~%Pushing updated branches...~%")
            (loop for commit in commits
                  for patch-id = (getf commit :patch-id)
                  for branch-name = (smoke-branch-name patch-id)
                  do (create-branch branch-name (getf commit :hash))
                     (safe-push-branch branch-name)
                     (format t "  ~A -> ~A~%" (getf commit :short) branch-name))

            ;; Reopen closed PRs
            (when closed-prs
              (format t "~%Reopening closed PRs...~%")
              (loop for (patch-id . pr) in closed-prs
                    ;; Only reopen if the commit still exists in stack
                    when (find patch-id commits :key (lambda (c) (getf c :patch-id)) :test #'string=)
                      do (handler-case
                             (progn
                               (gh-pr-reopen pr)
                               (format t "  PR #~D reopened~%" pr))
                           (error ()
                             (format t "  PR #~D could not be reopened~%" pr)))))

            ;; Update draft states (all PRs already base on main)
            (format t "~%Updating PRs...~%")
            (loop for commit in commits
                  for i from 0
                  for patch-id = (getf commit :patch-id)
                  for pr = (find-pr-for-patch-id state patch-id)
                  when pr
                    do (let ((pr-st (pr-state pr)))
                         (when (eq pr-st :open)
                           ;; Update draft state - only first PR is ready
                           (if (zerop i)
                               (progn
                                 (ignore-errors (gh-pr-ready pr))
                                 (format t "  PR #~D marked ready~%" pr))
                               (progn
                                 (ignore-errors (gh-pr-draft pr))
                                 (format t "  PR #~D marked draft~%" pr))))))

            ;; Check for commits without PRs
            (let ((missing-prs (loop for commit in commits
                                     for patch-id = (getf commit :patch-id)
                                     unless (find-pr-for-patch-id state patch-id)
                                       collect commit)))
              (when missing-prs
                (format t "~%~D commit~:P need~:[s~;~] PRs. Run 'smoke push' to create them.~%"
                        (length missing-prs)
                        (= (length missing-prs) 1)))))))

    (save-state state)
    (format t "~%Done.~%")))

(defun url-stack (&optional position)
  "Show PR URL(s). If POSITION given (1-indexed), show that commit's PR URL."
  (let* ((commits (stack-commits))
         (state (load-state)))
    (when (null commits)
      (format t "No commits in stack.~%")
      (return-from url-stack))
    (if position
        ;; Show URL for specific position
        (if (or (< position 1) (> position (length commits)))
            (format t "Invalid position ~D. Stack has ~D commit~:P.~%"
                    position (length commits))
            (let* ((commit (nth (1- position) commits))
                   (patch-id (getf commit :patch-id))
                   (pr (find-pr-for-patch-id state patch-id)))
              (if pr
                  (let ((url (gh-pr-url pr)))
                    (if url
                        (format t "~A~%" url)
                        (format t "https://github.com/~A/pull/~D~%"
                                (repo-name) pr)))
                  (format t "No PR for commit ~A (~A)~%"
                          (getf commit :short) (getf commit :subject)))))
        ;; Show all URLs
        (loop for commit in commits
              for i from 1
              for patch-id = (getf commit :patch-id)
              for pr = (find-pr-for-patch-id state patch-id)
              do (if pr
                     (let ((url (gh-pr-url pr)))
                       (format t "~D. ~A ~A~%   ~A~%"
                               i (getf commit :short) (getf commit :subject)
                               (or url (format nil "PR #~D" pr))))
                     (format t "~D. ~A ~A~%   (no PR)~%"
                             i (getf commit :short) (getf commit :subject)))))))

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
