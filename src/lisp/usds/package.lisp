;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :usds
  (:use #:cl #:cffi)
  (:export
   #:*usds-ns-package*
   #:unknown-form-error
   #:write-to-layer
   #:write-to-layer-call-enabled))

(uiop:define-package #:usds-ns
  (:use-reexport #:mopr-ns))
