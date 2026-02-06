;;;; Build script for smoke

(require :asdf)

;; Load quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Add current directory to ASDF source registry
(push '*default-pathname-defaults* asdf:*central-registry*)

;; Load dependencies
(ql:quickload '(:alexandria :serapeum :str :cl-json :cl-ppcre) :silent t)

;; Load the system
(asdf:load-system :smoke)

;; Build executable
(sb-ext:save-lisp-and-die "smoke"
                          :toplevel #'smoke:main
                          :executable t
                          :compression t
                          :save-runtime-options t)
