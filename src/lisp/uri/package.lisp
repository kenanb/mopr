;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-uri
  (:use #:cl)

  (:export

   ;; DESCRIPTOR
   #:make-descriptor
   #:make-descriptor-for-path
   #:descriptor-p
   #:descriptor
   #:descriptor-uuid
   #:descriptor-path

   #:rchain-descriptor-uuids

   #:descriptor-alist-assoc-by-data
   #:descriptor-alist-assoc-by-uuid
   #:descriptor-alist-assoc-by-path
   #:descriptor-alist-assoc

   ))
