;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-exe
  (:use #:cl
        #:mopr-sgt ; TODO : Revise.
        )
  (:export

   ;; PROCEDURE API
   #:procedure-execute

   ))
