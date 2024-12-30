;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-srv
  (:use #:cl)
  (:export

   #:in-process-backend-init
   #:in-process-backend-term
   #:in-process-backend-handle-get-request
   #:in-process-backend-release-response

   ))
