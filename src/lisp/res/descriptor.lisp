;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defstruct (descriptor
            (:constructor)
            (:constructor make-descriptor-for-file
                (file
                 &aux
                   (uuid (frugal-uuid:to-string (frugal-uuid:make-v7)))
                   (path (or (file-pathname-p file)
                             (error "File descriptor requested for non-file path!")))))
            (:constructor make-descriptor-for-directory
                (directory
                 &aux
                   (uuid (frugal-uuid:to-string (frugal-uuid:make-v7)))
                   (path (ensure-directory-pathname directory)))))
  "DESCRIPTOR

A descriptor represents the means to unambiguously refer to a resource or
resource grouping.

Every entity that has an associated descriptor will be associated with a UUIDv7
at creation-time, so that the underlying content can be moved while maintaining
stable addressing by the client application.
"
  (uuid (error "DESCRIPTOR cannot be initialized without a uuid!")
   :type (simple-base-string 36)
   :read-only t)
  (path (error "DESCRIPTOR cannot be initialized without a path!")
   :type pathname
   :read-only t))
