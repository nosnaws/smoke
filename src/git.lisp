(in-package :smoke)

;;; Git operations

(defun run-git (&rest args)
  "Run git command with ARGS, return trimmed output."
  (str:trim
   (uiop:run-program (cons "git" args)
                     :output '(:string :stripped t)
                     :error-output :interactive)))

(defun run-git-lines (&rest args)
  "Run git command with ARGS, return list of output lines."
  (let ((output (apply #'run-git args)))
    (if (str:empty? output)
        nil
        (str:lines output))))

(defun current-branch ()
  "Return the current branch name."
  (run-git "rev-parse" "--abbrev-ref" "HEAD"))

(defun main-branch ()
  "Return the main branch name (main or master)."
  (let ((branches (run-git-lines "branch" "--list" "main" "master")))
    ;; Strip leading "* " from current branch indicator and whitespace
    (let ((clean-branches (mapcar (lambda (b)
                                    (str:trim (string-left-trim '(#\* #\Space) b)))
                                  branches)))
      (or (find "main" clean-branches :test #'string=)
          (find "master" clean-branches :test #'string=)
          "main"))))

(defun merge-base (branch)
  "Return the merge base commit between current branch and BRANCH."
  (run-git "merge-base" "HEAD" branch))

(defun commits-since (base)
  "Return list of commit hashes from BASE to HEAD (oldest first)."
  (reverse (run-git-lines "rev-list" (format nil "~A..HEAD" base))))

(defun commit-subject (commit)
  "Return the subject line of COMMIT."
  (run-git "log" "-1" "--format=%s" commit))

(defun commit-short-hash (commit)
  "Return the short hash of COMMIT."
  (run-git "log" "-1" "--format=%h" commit))

(defun remote-main-branch ()
  "Return the remote main branch ref (origin/main or origin/master)."
  (format nil "origin/~A" (main-branch)))

(defun stack-commits ()
  "Return list of commits in the current stack (oldest first).
Each commit is a plist with :hash, :short, and :subject."
  (let* ((base (merge-base (remote-main-branch)))
         (commits (commits-since base)))
    (mapcar (lambda (hash)
              (list :hash hash
                    :short (commit-short-hash hash)
                    :subject (commit-subject hash)))
            commits)))

(defun ensure-clean-worktree ()
  "Signal an error if the worktree has uncommitted changes."
  (let ((status (run-git "status" "--porcelain")))
    (unless (str:empty? status)
      (error "Working tree has uncommitted changes. Commit or stash them first."))))

(define-condition rebase-conflict (error)
  ((target :initarg :target
           :reader rebase-conflict-target
           :type string
           :documentation "The rebase target ref."))
  (:report (lambda (c stream)
             (format stream "Rebase onto ~A has conflicts.~%~
                             Resolve conflicts and run: git rebase --continue~%~
                             Or abort the rebase with: git rebase --abort"
                     (rebase-conflict-target c))))
  (:documentation "Signaled when a rebase encounters conflicts."))

(defun rebase-in-progress-p ()
  "Return t if a rebase is currently in progress."
  (let ((git-dir (run-git "rev-parse" "--git-dir")))
    (uiop:file-exists-p (merge-pathnames "REBASE_HEAD" (uiop:parse-unix-namestring git-dir :ensure-directory t)))))

(defun rebase-onto (target)
  "Rebase the current branch onto TARGET.
Signals REBASE-CONFLICT if the rebase encounters conflicts."
  (handler-case
      (run-git "rebase" target)
    (error ()
      (if (rebase-in-progress-p)
          (error 'rebase-conflict :target target)
          (error "Rebase onto ~A failed." target)))))

(defun push-force-with-lease (branch)
  "Force push with lease to BRANCH."
  (run-git "push" "--force-with-lease" "origin" branch))

(defun push-force (branch)
  "Force push to BRANCH. Use when branch may not exist on remote."
  (run-git "push" "--force" "origin" branch))

(defun safe-push-branch (branch)
  "Push BRANCH to remote, handling both new and existing branches."
  (handler-case
      (push-force-with-lease branch)
    (error ()
      ;; If force-with-lease fails (branch doesn't exist), use regular force
      (push-force branch))))

(defun create-branch (name commit)
  "Create or update branch NAME pointing to COMMIT."
  (run-git "branch" "-f" name commit))

(defun delete-remote-branch (name)
  "Delete remote branch NAME."
  (ignore-errors
    (run-git "push" "origin" "--delete" name)))

(defun fetch-main ()
  "Fetch the main branch from origin."
  (run-git "fetch" "origin" (main-branch)))

(defun repo-name ()
  "Return the owner/repo name from the remote URL."
  (let ((url (run-git "remote" "get-url" "origin")))
    ;; Handle SSH (git@github.com:owner/repo.git) or HTTPS (https://github.com/owner/repo.git)
    (multiple-value-bind (match groups)
        (re:scan-to-strings "github\\.com[:/](.+?)(?:\\.git)?$" url)
      (declare (ignore match))
      (when groups
        (str:replace-all ".git" "" (aref groups 0))))))

(defun interactive-pick-commit (commits)
  "Let user pick a commit from COMMITS. Returns the selected commit plist."
  (format t "~%Select a commit to amend:~%~%")
  (loop for commit in commits
        for i from 1
        do (format t "  ~D. ~A ~A~%"
                   i
                   (getf commit :short)
                   (getf commit :subject)))
  (format t "~%Enter number (1-~D): " (length commits))
  (finish-output)
  (let ((choice (parse-integer (read-line) :junk-allowed t)))
    (if (and choice (<= 1 choice (length commits)))
        (nth (1- choice) commits)
        (progn
          (format t "Invalid choice.~%")
          (interactive-pick-commit commits)))))

(defun amend-commit (target-commit)
  "Amend TARGET-COMMIT with current staged changes using interactive rebase."
  (let ((target-hash (getf target-commit :hash)))
    ;; Start interactive rebase with edit on target commit
    (uiop:run-program
     (list "git" "rebase" "-i" "--autosquash"
           (format nil "~A^" target-hash))
     :input :interactive
     :output :interactive
     :error-output :interactive)))
