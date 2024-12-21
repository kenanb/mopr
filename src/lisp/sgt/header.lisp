;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (header (:copier nil))
  (node-type 'cnode :type symbol)
  (metadata nil :type list)
  (payloads nil :type list))

(defun clone-header (header &key (node-type t) (metadata t))
  "CLONE-HEADER

Creates a partial clone of the header. Any shared content is assumed as-if
immutable.

ASSUMPTION: A PAYLOAD registered to a PROCEDURE is never visibly modified,
            including recursive modifications.

As long as PAYLOADS (and associated digest strings) are treated as-if immutable,
COPY-LIST should be sufficient to maintain isolation across PROCEDURE instances.
"
  (make-header :node-type (if (eq t node-type) (header-node-type header) node-type)
               :metadata (if (eq t metadata) (copy-alist (header-metadata header)) metadata)
               :payloads (copy-list (header-payloads header))))

(defvar *header* nil)

(defconstant +digest-prefix-size+ 8)

(defun digest-prefix= (x y)
  (string= x y :end1 +digest-prefix-size+
               :end2 +digest-prefix-size+))

(defun find-payload-in-bound-header (digest)
  (unless *header* (error "FIND-PAYLOAD-IN-BOUND-HEADER called without a header bound."))
  (cdr (assoc digest (header-payloads *header*) :test #'digest-prefix=)))

(defun register-payload-to-bound-header (p &aux (digest (payload-calculate-digest p)))
  (unless *header* (error "REGISTER-PAYLOAD-TO-BOUND-HEADER called without a header bound."))
  (setf (header-payloads *header*) (acons digest p (header-payloads *header*)))
  (subseq digest 0 +digest-prefix-size+))
