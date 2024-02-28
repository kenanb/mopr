;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-plug
  (:use #:cl #:mopr)

  ;; CONFIG
  (:export
   #:*config*
   #:configuration
   #:configure
   #:create-data-call-table
   #:create-prim-call-table
   #:with-configuration)

  (:export
   #:callable
   #:process-data-call-stack
   #:process-prim-call-stack))
