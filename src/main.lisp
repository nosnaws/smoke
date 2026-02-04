(in-package :smoke)

;;; CLI entry point

(defun print-usage ()
  "Print usage information."
  (format t "smoke - manage stacked diffs with squash-merge workflows~%~%")
  (format t "Usage:~%")
  (format t "  smoke         Show stack status~%")
  (format t "  smoke push    Create/update PRs for each commit~%")
  (format t "  smoke pull    Rebase onto main, update PRs~%")
  (format t "  smoke amend   Pick a commit to amend~%")
  (format t "  smoke help    Show this help~%"))

(defun main ()
  "CLI entry point."
  (let ((args (uiop:command-line-arguments)))
    (handler-case
        (cond
          ((null args)
           (status))
          ((string= (first args) "push")
           (push-stack))
          ((string= (first args) "pull")
           (pull-stack))
          ((string= (first args) "amend")
           (amend-stack))
          ((or (string= (first args) "help")
               (string= (first args) "--help")
               (string= (first args) "-h"))
           (print-usage))
          (t
           (format *error-output* "Unknown command: ~A~%" (first args))
           (print-usage)
           (uiop:quit 1)))
      (error (e)
        (format *error-output* "Error: ~A~%" e)
        (uiop:quit 1)))))

;;; REPL helpers for development

(defun dev-status ()
  "Show status (for REPL use)."
  (status))

(defun dev-push ()
  "Push stack (for REPL use)."
  (push-stack))

(defun dev-pull ()
  "Pull and rebase (for REPL use)."
  (pull-stack))
