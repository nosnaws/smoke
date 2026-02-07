(in-package :smoke)

;;; State management
;;; State is stored in .smoke/state.json relative to repo root

(defun repo-root ()
  "Return the git repository root directory."
  (str:trim (run-git "rev-parse" "--show-toplevel")))

(defun state-dir ()
  "Return the .smoke directory path."
  (merge-pathnames ".smoke/" (uiop:parse-unix-namestring (repo-root) :ensure-directory t)))

(defun state-file ()
  "Return the state.json file path."
  (merge-pathnames "state.json" (state-dir)))

(defun ensure-state-dir ()
  "Create the .smoke directory if it doesn't exist."
  (ensure-directories-exist (state-dir)))

(defun migrate-state-entry (entry branch position)
  "Migrate a legacy state ENTRY to index-based format.
BRANCH is the stack's base branch name. POSITION is the 0-based index.
Handles:
  - Legacy entries with :patch--id (no :smoke--branch): convert to index-based
  - Entries with :index but no :smoke--branch: add smoke-branch
  - Entries already having :smoke--branch: keep as-is"
  (let ((has-smoke-branch (assoc :smoke--branch entry))
        (has-index (assoc :index entry))
        (pr (cdr (assoc :pr entry)))
        (smoke-branch (smoke-branch-name branch (1+ position))))
    (cond
      ;; Already fully migrated
      (has-smoke-branch entry)
      ;; Has index but no smoke-branch
      (has-index
       (list (cons :index position)
             (cons :pr pr)
             (cons :smoke--branch smoke-branch)))
      ;; Legacy patch-id entry
      (t
       (list (cons :index position)
             (cons :pr pr)
             (cons :smoke--branch smoke-branch))))))

(defun migrate-state (state)
  "Migrate all stack entries in STATE to index-based format."
  (let ((branch (cdr (assoc :branch state)))
        (stack (cdr (assoc :stack state))))
    (list (cons :branch branch)
          (cons :stack (loop for e in stack
                             for i from 0
                             collect (migrate-state-entry e branch i))))))

(defun load-state ()
  "Load state from .smoke/state.json. Returns alist or nil."
  (let ((path (state-file)))
    (when (uiop:file-exists-p path)
      (migrate-state (json:decode-json-from-source path)))))

(defun state-tmp-file ()
  "Return the temporary state file path."
  (merge-pathnames "state.json.tmp" (state-dir)))

(defun save-state (state)
  "Save STATE to .smoke/state.json atomically via temp file + rename."
  (ensure-state-dir)
  (let ((tmp (state-tmp-file))
        (target (state-file)))
    (with-open-file (out tmp
                         :direction :output
                         :if-exists :supersede)
      (format out "{~%")
      (format out "  \"branch\": ~S,~%"
              (cdr (assoc :branch state)))
      (format out "  \"stack\": [~%")
      (let ((stack (cdr (assoc :stack state))))
        (loop for entry in stack
              for i from 0
              for smoke-branch = (cdr (assoc :smoke--branch entry))
              do (format out "    {\"index\": ~D, \"pr\": ~D~@[, \"smoke_branch\": ~S~]}"
                         (cdr (assoc :index entry))
                         (cdr (assoc :pr entry))
                         smoke-branch)
              when (< i (1- (length stack)))
                do (format out ",")
              do (format out "~%")))
      (format out "  ]~%")
      (format out "}~%"))
    (rename-file tmp target)))

(defun init-state (branch)
  "Create initial state for BRANCH."
  (list (cons :branch branch)
        (cons :stack nil)))

(defun remove-merged-prs-from-state (state merged-pr-numbers)
  "Remove entries with PR numbers in MERGED-PR-NUMBERS from STATE."
  (let ((branch (cdr (assoc :branch state)))
        (stack (cdr (assoc :stack state))))
    (list (cons :branch branch)
          (cons :stack
                (remove-if (lambda (e)
                             (member (cdr (assoc :pr e)) merged-pr-numbers))
                           stack)))))

;;; Reconciliation — match commits to state entries

(defun reconcile-stack (commits state)
  "Match COMMITS against STATE stack entries by position.
Returns list of plists: (:commit c :pr n :branch b :position pos).
Also returns as second value a list of orphaned state entries (unmatched)."
  (let* ((stack (cdr (assoc :stack state)))
         (results (loop for commit in commits
                        for ci from 0
                        for entry = (nth ci stack)
                        collect (if entry
                                    (list :commit commit
                                          :pr (cdr (assoc :pr entry))
                                          :branch (cdr (assoc :smoke--branch entry))
                                          :position (1+ ci))
                                    (list :commit commit
                                          :pr nil
                                          :branch nil
                                          :position (1+ ci)))))
         (orphans (when (> (length stack) (length commits))
                    (subseq stack (length commits)))))
    (values results orphans)))

(defun build-state-from-reconciliation (branch reconciled)
  "Build a state alist from BRANCH name and RECONCILED results."
  (list (cons :branch branch)
        (cons :stack
              (loop for r in reconciled
                    for i from 0
                    when (getf r :pr)
                      collect (list (cons :index i)
                                    (cons :pr (getf r :pr))
                                    (cons :smoke--branch (getf r :branch)))))))
