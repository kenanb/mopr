;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-plug
  (:use #:cl #:mopr)

  ;; CONFIG
  (:export
   #:*default-config-system*
   #:*config*
   #:configuration
   #:configure)

  (:export
   #:callable
   #:process-call-stack
   #:create-data-call-table
   #:create-prim-call-table))
