;;;; package.lisp

(defpackage :mopr-exe
  (:use #:cl
        #:mopr-sgt ; TODO : Revise.
        )
  (:export

   ;; PROCEDURE API
   #:procedure-execute

   ))
