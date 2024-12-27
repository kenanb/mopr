;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-uri
  (:use #:cl)

  (:export

   ;; DESCRIPTOR
   #:make-descriptor
   #:make-new-descriptor
   #:descriptor-p
   #:descriptor
   #:descriptor-role
   #:descriptor-uuid

   #:rchain-descriptor-uuids

   #:new-uuid

   #:descriptor-alist-assoc

   ;; Below are only exposed to use in DESCRIPTOR extensions.
   #:%descriptor-alist-assoc-by-data
   #:%descriptor-alist-assoc-by-uuid

   ))
