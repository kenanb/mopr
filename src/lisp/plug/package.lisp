;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-plug
  (:use #:cl #:mopr)
  (:export
   #:process-call-stack
   #:create-data-call-table
   #:create-prim-call-table))
