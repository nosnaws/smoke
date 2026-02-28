(in-package :smoke)

;;; CLI entry point

(defun print-usage ()
  "Print usage information."
  (format t "smoke - manage stacked diffs with squash-merge workflows~%~%")
  (format t "Usage:~%")
  (format t "  smoke         Show stack status~%")
  (format t "  smoke push [--all]  Create/update PR for first commit (--all for entire stack)~%")
  (format t "  smoke pull    Rebase onto main, update PRs~%")
  (format t "  smoke amend   Interactive rebase to amend stack~%")
  (format t "  smoke url [N] Show PR URL(s), optionally for stack position N~%")
  (format t "  smoke help    Show this help~%"))

(defun main ()
  "CLI entry point."
  (let ((args (uiop:command-line-arguments)))
    (handler-case
        (cond
          ((null args)
           (status))
          ((string= (first args) "push")
           (push-stack :all (member "--all" (rest args) :test #'string=)))
          ((string= (first args) "pull")
           (pull-stack))
          ((string= (first args) "amend")
           (amend-stack))
          ((string= (first args) "url")
           (let ((pos (when (second args)
                        (parse-integer (second args) :junk-allowed t))))
             (url-stack pos)))
          ((or (string= (first args) "help")
               (string= (first args) "--help")
               (string= (first args) "-h"))
           (print-usage))
          ((or (string= (first args) "version")
               (string= (first args) "--version")
               (string= (first args) "-v"))
           (format t "smoke ~A~%" (asdf:component-version (asdf:find-system :smoke))))
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

(defun dev-push (&key all)
  "Push stack (for REPL use)."
  (push-stack :all all))

(defun dev-pull ()
  "Pull and rebase (for REPL use)."
  (pull-stack))
