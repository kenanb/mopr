;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-uri)

(defstruct (descriptor
            (:constructor)
            (:constructor make-descriptor-for-path
                (path &aux (uuid (frugal-uuid:to-string (frugal-uuid:make-v7))))))
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

(defun rchain-descriptor-uuids (descriptors)
  "RCHAIN-DESCRIPTOR-UUIDS

Chains the uuids of each descriptor, in REVERSE order. EXAMPLE:

Merging a list of descriptors (CD BD AD) that would have the uuids ('2' '1' '0'),
correspondingly, will generate '0/1/2'.
"
  (format nil "~{~A~^/~}" (reverse (mapcar #'descriptor-uuid descriptors))))

(defun descriptor-alist-assoc-by-data (desc-alist val)
  (rassoc val desc-alist))

(defun descriptor-alist-assoc-by-uuid (desc-alist val)
  (assoc val desc-alist
         :key #'descriptor-uuid
         :test #'equal))

(defun descriptor-alist-assoc-by-path (desc-alist val)
  (assoc val desc-alist
         :key #'descriptor-path
         :test #'equal))

(defun descriptor-alist-assoc (desc-alist lookup-type lookup-val)
  (let ((lookup-fn (case lookup-type
                     (:data #'descriptor-alist-assoc-by-data)
                     (:uuid #'descriptor-alist-assoc-by-uuid)
                     (:path #'descriptor-alist-assoc-by-path)
                     (otherwise (error "Unknown DESCRIPTOR-ALIST lookup type!")))))
    (funcall lookup-fn desc-alist lookup-val)))
