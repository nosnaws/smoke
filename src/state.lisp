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

(defun load-state ()
  "Load state from .smoke/state.json. Returns alist or nil."
  (let ((path (state-file)))
    (when (uiop:file-exists-p path)
      (json:decode-json-from-source path))))

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
              do (format out "    {\"patch_id\": ~S, \"pr\": ~D}"
                         (cdr (assoc :patch--id entry))
                         (cdr (assoc :pr entry)))
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

(defun update-state-mapping (state patch-id pr-number)
  "Return new state with PATCH-ID mapped to PR-NUMBER."
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
                                        (cons :pr pr-number))
                                  e))
                            stack)))
        ;; Add new entry
        (list (cons :branch branch)
              (cons :stack
                    (append stack
                            (list (list (cons :patch--id patch-id)
                                        (cons :pr pr-number)))))))))

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
