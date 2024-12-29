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

A descriptor represents the means to unambiguously refer to a resource, within
the scope of its immediate parent resource. It is used in addressing resources,
in various ways, by various subsystems.

 The resource can be:

- A grouping of assets: a workshop, or a project.

- An asset.

- A component within an asset's persistent representation.

- A transient component within an asset's runtime representation.

Any content that has an associated descriptor will be associated with a UUID
at creation-time. The UUID version used depends on the nature of asset.
"
  (role (error "DESCRIPTOR cannot be initialized without a role!")
   :type keyword
   :read-only t)
  (uuid (error "DESCRIPTOR cannot be initialized without a uuid!")
   :type (simple-base-string 36)
   :read-only t))
