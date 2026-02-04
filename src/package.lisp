(defpackage :smoke
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:s :serapeum)
                    (:re :cl-ppcre))
  (:export :main
           :status
           :push-stack
           :pull-stack
           :amend-stack
           :url-stack))
