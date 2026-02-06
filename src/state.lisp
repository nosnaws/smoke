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

(defun migrate-state-entry (entry branch)
  "Migrate a legacy state ENTRY (no :smoke--branch) by reconstructing the old
patch-id-based branch name. BRANCH is the stack's base branch name."
  (if (assoc :smoke--branch entry)
      entry
      (let* ((patch-id (cdr (assoc :patch--id entry)))
             (prefix (subseq patch-id 0 (min 8 (length patch-id))))
             (reconstructed (format nil "smoke/~A/~A" branch prefix)))
        (cons (cons :smoke--branch reconstructed) entry))))

(defun migrate-state (state)
  "Migrate all stack entries in STATE, adding :smoke--branch where missing."
  (let ((branch (cdr (assoc :branch state)))
        (stack (cdr (assoc :stack state))))
    (list (cons :branch branch)
          (cons :stack (mapcar (lambda (e) (migrate-state-entry e branch))
                               stack)))))

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
              do (format out "    {\"patch_id\": ~S, \"pr\": ~D~@[, \"smoke_branch\": ~S~]}"
                         (cdr (assoc :patch--id entry))
                         (cdr (assoc :pr entry))
                         smoke-branch)
              when (< i (1- (length stack)))
                do (format out ",")
              do (format out "~%")))
      (format out "  ]~%")
      (format out "}~%"))
    (rename-file tmp target)))

(defun find-pr-for-patch-id (state patch-id)
  "Find the PR number for PATCH-ID in STATE, or nil."
  (let ((stack (cdr (assoc :stack state))))
    (loop for entry in stack
          when (string= (cdr (assoc :patch--id entry)) patch-id)
            return (cdr (assoc :pr entry)))))

(defun update-state-mapping (state patch-id pr-number smoke-branch)
  "Return new state with PATCH-ID mapped to PR-NUMBER and SMOKE-BRANCH."
  (let* ((branch (cdr (assoc :branch state)))
         (stack (cdr (assoc :stack state)))
         (existing (find patch-id stack
                         :key (lambda (e) (cdr (assoc :patch--id e)))
                         :test #'string=)))
    (if existing
        ;; Update existing entry
        (list (cons :branch branch)
              (cons :stack
                    (mapcar (lambda (e)
                              (if (string= (cdr (assoc :patch--id e)) patch-id)
                                  (list (cons :patch--id patch-id)
                                        (cons :pr pr-number)
                                        (cons :smoke--branch smoke-branch))
                                  e))
                            stack)))
        ;; Add new entry
        (list (cons :branch branch)
              (cons :stack
                    (append stack
                            (list (list (cons :patch--id patch-id)
                                        (cons :pr pr-number)
                                        (cons :smoke--branch smoke-branch)))))))))

(defun init-state (branch)
  "Create initial state for BRANCH."
  (list (cons :branch branch)
        (cons :stack nil)))

(defun remove-merged-from-state (state merged-patch-ids)
  "Remove MERGED-PATCH-IDS from STATE."
  (let ((branch (cdr (assoc :branch state)))
        (stack (cdr (assoc :stack state))))
    (list (cons :branch branch)
          (cons :stack
                (remove-if (lambda (e)
                             (member (cdr (assoc :patch--id e)) merged-patch-ids
                                     :test #'string=))
                           stack)))))

;;; Reconciliation — match commits to state entries

(defun reconcile-stack (commits state)
  "Match COMMITS (list of plists with :patch-id) against STATE stack entries.
Two-pass algorithm:
  1. Match by patch ID (handles rebases/reorders)
  2. Match remaining commits to remaining entries by position (handles amends)
Returns list of plists: (:commit c :pr n :branch b :patch-id id :position pos).
Also returns as second value a list of orphaned state entries (unmatched)."
  (let* ((stack (cdr (assoc :stack state)))
         (matched-state-indices (make-hash-table))
         (matched-commit-indices (make-hash-table))
         (results (make-array (length commits) :initial-element nil)))
    ;; Pass 1: match by patch ID
    (loop for commit in commits
          for ci from 0
          for patch-id = (getf commit :patch-id)
          do (loop for entry in stack
                   for si from 0
                   when (and (not (gethash si matched-state-indices))
                             (string= (cdr (assoc :patch--id entry)) patch-id))
                     do (setf (gethash si matched-state-indices) t
                              (gethash ci matched-commit-indices) t
                              (aref results ci)
                              (list :commit commit
                                    :pr (cdr (assoc :pr entry))
                                    :branch (cdr (assoc :smoke--branch entry))
                                    :patch-id patch-id
                                    :position (1+ ci)))
                     and return nil))
    ;; Pass 2: position-based fallback for unmatched commits
    (loop for commit in commits
          for ci from 0
          unless (gethash ci matched-commit-indices)
            do (loop for entry in stack
                     for si from 0
                     ;; Match by position: commit index ci matches state index ci
                     when (and (= si ci)
                               (not (gethash si matched-state-indices)))
                       do (setf (gethash si matched-state-indices) t
                                (aref results ci)
                                (list :commit commit
                                      :pr (cdr (assoc :pr entry))
                                      :branch (cdr (assoc :smoke--branch entry))
                                      :patch-id (getf commit :patch-id)
                                      :position (1+ ci)))
                       and return nil))
    ;; Fill in unmatched commits (no state entry found)
    (loop for commit in commits
          for ci from 0
          unless (aref results ci)
            do (setf (aref results ci)
                     (list :commit commit
                           :pr nil
                           :branch nil
                           :patch-id (getf commit :patch-id)
                           :position (1+ ci))))
    ;; Collect orphaned state entries
    (let ((orphans (loop for entry in stack
                         for si from 0
                         unless (gethash si matched-state-indices)
                           collect entry)))
      (values (coerce results 'list) orphans))))

(defun build-state-from-reconciliation (branch reconciled)
  "Build a state alist from BRANCH name and RECONCILED results."
  (list (cons :branch branch)
        (cons :stack
              (loop for r in reconciled
                    when (getf r :pr)
                      collect (list (cons :patch--id (getf r :patch-id))
                                    (cons :pr (getf r :pr))
                                    (cons :smoke--branch (getf r :branch)))))))
