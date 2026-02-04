(in-package :smoke)

;;; GitHub CLI operations

(defun run-gh (&rest args)
  "Run gh command with ARGS, return trimmed output."
  (str:trim
   (uiop:run-program (cons "gh" args)
                     :output '(:string :stripped t)
                     :error-output :interactive)))

(defun run-gh-json (&rest args)
  "Run gh command with ARGS, parse JSON output."
  (let ((output (apply #'run-gh args)))
    (when (and output (not (str:empty? output)))
      (json:decode-json-from-string output))))

(defun gh-pr-list ()
  "List all open PRs for current repo created by current user."
  (run-gh-json "pr" "list" "--author" "@me" "--json" "number,title,headRefName,isDraft,state"))

(defun gh-pr-view (pr-number)
  "Get PR details by number."
  (run-gh-json "pr" "view" (princ-to-string pr-number)
               "--json" "number,title,headRefName,isDraft,state,mergeCommit,statusCheckRollup"))

(defun extract-pr-number (url)
  "Extract PR number from GitHub PR URL."
  (multiple-value-bind (match groups)
      (re:scan-to-strings "/pull/(\\d+)" url)
    (declare (ignore match))
    (when groups
      (parse-integer (aref groups 0)))))

(defun gh-pr-create (title body base-branch head-branch &key draft)
  "Create a new PR. Returns PR number."
  (let* ((args (list "pr" "create"
                     "--title" title
                     "--body" body
                     "--base" base-branch
                     "--head" head-branch))
         (args (if draft (append args '("--draft")) args))
         (output (apply #'run-gh args)))
    ;; gh pr create outputs the PR URL, extract number from it
    (extract-pr-number output)))

(defun gh-pr-create-simple (title base-branch head-branch &key draft)
  "Create a new PR with minimal body. Returns PR number."
  (let* ((args (list "pr" "create"
                     "--title" title
                     "--body" ""
                     "--base" base-branch
                     "--head" head-branch))
         (args (if draft (append args '("--draft")) args))
         (output (apply #'run-gh args)))
    ;; gh pr create outputs the PR URL
    (extract-pr-number output)))

(defun gh-pr-ready (pr-number)
  "Mark PR as ready for review (not draft)."
  (run-gh "pr" "ready" (princ-to-string pr-number)))

(defun gh-pr-draft (pr-number)
  "Convert PR to draft."
  ;; gh doesn't have a direct --draft flag for existing PRs
  ;; We use the API directly
  (run-gh "pr" "ready" (princ-to-string pr-number) "--undo"))

(defun gh-pr-edit-base (pr-number new-base)
  "Change the base branch of a PR."
  (run-gh "pr" "edit" (princ-to-string pr-number) "--base" new-base))

(defun pr-state (pr-number)
  "Return PR state: :open, :merged, :closed, or nil on error."
  (let ((pr (ignore-errors (gh-pr-view pr-number))))
    (when pr
      (let ((state (cdr (assoc :state pr))))
        (cond
          ((string= state "MERGED") :merged)
          ((string= state "CLOSED") :closed)
          ((string= state "OPEN") :open)
          (t nil))))))

(defun pr-merged-p (pr-number)
  "Return t if PR is merged."
  (eq (pr-state pr-number) :merged))

(defun pr-closed-p (pr-number)
  "Return t if PR is closed (not merged)."
  (eq (pr-state pr-number) :closed))

(defun pr-open-p (pr-number)
  "Return t if PR is open."
  (eq (pr-state pr-number) :open))

(defun pr-draft-p (pr-number)
  "Return t if PR is a draft."
  (let ((pr (ignore-errors (gh-pr-view pr-number))))
    (when pr
      (cdr (assoc :is-draft pr)))))

(defun pr-ci-status (pr-number)
  "Return CI status for PR: :success, :failure, :pending, or :none."
  (let* ((pr (gh-pr-view pr-number))
         (checks (cdr (assoc :status-check-rollup pr))))
    (cond
      ((null checks) :none)
      ((every (lambda (c) (string= (cdr (assoc :conclusion c)) "SUCCESS")) checks)
       :success)
      ((some (lambda (c) (member (cdr (assoc :conclusion c))
                                 '("FAILURE" "ERROR" "CANCELLED")
                                 :test #'string=))
             checks)
       :failure)
      (t :pending))))

(defun format-ci-status (status)
  "Format CI status for display."
  (case status
    (:success "✓")
    (:failure "✗")
    (:pending "○")
    (:none "-")))

(defun smoke-branch-name (position)
  "Generate smoke branch name for stack position."
  (format nil "smoke/~A/~D" (current-branch) position))
