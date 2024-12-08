;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct header
  (payloads nil :type list))

(defun clone-header (iheader &aux (oheader (copy-header iheader)))
  "CLONE-HEADER

Creates a partial clone of the header. Any shared content is assumed as-if
immutable.

ASSUMPTION: A PAYLOAD registered to a PROCEDURE is never visibly modified,
            including recursive modifications.

As long as PAYLOADS (and associated digest strings) are treated as-if immutable,
COPY-LIST should be sufficient to maintain isolation across PROCEDURE instances.
"
  (setf (header-payloads oheader) (copy-list (header-payloads iheader)))
  oheader)

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
