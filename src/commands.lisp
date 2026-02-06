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
        (let ((reconciled (reconcile-stack commits state)))
          (format t "~%Stack on ~A (~D commit~:P ahead of ~A):~%~%"
                  branch (length commits) main)
          (loop for r in reconciled
                for commit = (getf r :commit)
                for pr = (getf r :pr)
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

    (multiple-value-bind (reconciled orphans) (reconcile-stack commits state)
      ;; For each reconciled commit, create a branch and PR
      (loop for r in reconciled
            for i from 0
            for commit = (getf r :commit)
            for patch-id = (getf r :patch-id)
            for existing-pr = (getf r :pr)
            for position = (getf r :position)
            for branch-name = (or (getf r :branch)
                                  (smoke-branch-name branch position))
            do
               ;; Store branch name back into reconciled result
               (setf (getf r :branch) branch-name)

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
                                    main
                                    branch-name
                                    :draft is-draft)))
                      (format t "  ~A ~A  PR #~D (created~A)~%"
                              (getf commit :short)
                              (getf commit :subject)
                              pr-num
                              (if is-draft ", draft" ""))
                      (setf (getf r :pr) pr-num))))))

      ;; Clean up orphan branches
      (loop for orphan in orphans
            for orphan-branch = (cdr (assoc :smoke-branch orphan))
            when orphan-branch
              do (format t "  Cleaning up orphan branch ~A~%" orphan-branch)
                 (delete-remote-branch orphan-branch))

      ;; Build and save new state
      (let ((new-state (build-state-from-reconciliation branch reconciled)))
        (save-state new-state)))

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

    ;; Get new commits after rebase and reconcile
    (let ((commits (stack-commits)))
      (if (null commits)
          (format t "~%Stack is empty - all PRs merged!~%")
          (let ((reconciled (reconcile-stack commits state)))
            ;; Push updated branches (needed before reopening PRs)
            (format t "~%Pushing updated branches...~%")
            (loop for r in reconciled
                  for commit = (getf r :commit)
                  for position = (getf r :position)
                  for branch-name = (or (getf r :branch)
                                        (smoke-branch-name branch position))
                  do (setf (getf r :branch) branch-name)
                     (create-branch branch-name (getf commit :hash))
                     (safe-push-branch branch-name)
                     (format t "  ~A -> ~A~%" (getf commit :short) branch-name))

            ;; Reopen closed PRs
            (when closed-prs
              (format t "~%Reopening closed PRs...~%")
              (loop for (patch-id . pr) in closed-prs
                    when (find patch-id reconciled
                                :key (lambda (r) (getf r :patch-id))
                                :test #'string=)
                      do (handler-case
                             (progn
                               (gh-pr-reopen pr)
                               (format t "  PR #~D reopened~%" pr))
                           (error ()
                             (format t "  PR #~D could not be reopened~%" pr)))))

            ;; Update draft states
            (format t "~%Updating PRs...~%")
            (loop for r in reconciled
                  for i from 0
                  for pr = (getf r :pr)
                  when pr
                    do (let ((pr-st (pr-state pr)))
                         (when (eq pr-st :open)
                           (if (zerop i)
                               (progn
                                 (ignore-errors (gh-pr-ready pr))
                                 (format t "  PR #~D marked ready~%" pr))
                               (progn
                                 (ignore-errors (gh-pr-draft pr))
                                 (format t "  PR #~D marked draft~%" pr))))))

            ;; Check for commits without PRs
            (let ((missing-prs (loop for r in reconciled
                                     unless (getf r :pr)
                                       collect (getf r :commit))))
              (when missing-prs
                (format t "~%~D commit~:P need~:[s~;~] PRs. Run 'smoke push' to create them.~%"
                        (length missing-prs)
                        (= (length missing-prs) 1))))

            ;; Save reconciled state
            (setf state (build-state-from-reconciliation branch reconciled)))))

    (save-state state)
    (format t "~%Done.~%")))

(defun url-stack (&optional position)
  "Show PR URL(s). If POSITION given (1-indexed), show that commit's PR URL."
  (let* ((branch (current-branch))
         (commits (stack-commits))
         (state (or (load-state) (init-state branch))))
    (when (null commits)
      (format t "No commits in stack.~%")
      (return-from url-stack))
    (let ((reconciled (reconcile-stack commits state)))
      (if position
          ;; Show URL for specific position
          (if (or (< position 1) (> position (length commits)))
              (format t "Invalid position ~D. Stack has ~D commit~:P.~%"
                      position (length commits))
              (let* ((r (nth (1- position) reconciled))
                     (commit (getf r :commit))
                     (pr (getf r :pr)))
                (if pr
                    (let ((url (gh-pr-url pr)))
                      (if url
                          (format t "~A~%" url)
                          (format t "https://github.com/~A/pull/~D~%"
                                  (repo-name) pr)))
                    (format t "No PR for commit ~A (~A)~%"
                            (getf commit :short) (getf commit :subject)))))
          ;; Show all URLs
          (loop for r in reconciled
                for commit = (getf r :commit)
                for i from 1
                for pr = (getf r :pr)
                do (if pr
                       (let ((url (gh-pr-url pr)))
                         (format t "~D. ~A ~A~%   ~A~%"
                                 i (getf commit :short) (getf commit :subject)
                                 (or url (format nil "PR #~D" pr))))
                       (format t "~D. ~A ~A~%   (no PR)~%"
                               i (getf commit :short) (getf commit :subject))))))))

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
