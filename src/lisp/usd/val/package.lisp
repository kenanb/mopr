;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-usd/val
  (:use #:cl)
  (:export
   #:+value-type-list+
   #:+value-role-list+
   #:get-real-type
   #:transfer-for-type
   #:get-transfer-for-type-function))
