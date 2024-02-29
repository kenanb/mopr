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
   #:create-call-tables
   #:with-configuration)

  (:export
   #:callable
   #:process-data-call-stack
   #:process-prim-call-stack))
