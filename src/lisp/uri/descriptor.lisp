;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-uri)

(defun new-uuid (data)
  (declare (ignore data))
  (frugal-uuid:to-string (frugal-uuid:make-v7)))

(defstruct (descriptor
            (:constructor)
            (:constructor make-new-descriptor (role data &aux (uuid (new-uuid data)))))
  "DESCRIPTOR

A descriptor represents the means to unambiguously refer to a resource or
resource grouping, as well as storing the role of the resource.

Any content that has an associated descriptor will be associated with a UUID
at creation-time. The UUID version used depends on the nature of resource.
"
  (role (error "DESCRIPTOR cannot be initialized without a role!")
   :type keyword
   :read-only t)
  (uuid (error "DESCRIPTOR cannot be initialized without a uuid!")
   :type (simple-base-string 36)
   :read-only t))
