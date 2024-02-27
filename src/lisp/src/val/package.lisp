;;;; package.lisp

(defpackage :mopr-val
  (:use #:cl)
  (:export
   #:+value-type-list+
   #:+value-role-list+
   #:get-real-type
   #:transfer-for-type
   #:get-transfer-for-type-function))
