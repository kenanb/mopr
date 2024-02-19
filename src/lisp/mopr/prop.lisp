;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-prop)

(defun prop-name-string (r-prop-name &key reverse-p)
  (format nil "~{~A~^:~}" (if reverse-p
                              (reverse r-prop-name)
                              r-prop-name)))

(defclass prop-info ()
  ((namespace
    :type list
    :initarg :namespace
    :initform nil
    :reader prop-info-namespace
    :documentation "...")
   (base-name
    :type base-string
    :initarg :base-name
    :initform (error "...")
    :reader prop-info-base-name
    :documentation "...")
   (full-name
    :type base-string
    :reader prop-info-full-name
    :documentation "...")
   (meta
    :type list
    :initarg :meta
    :initform nil
    :reader prop-info-meta
    :documentation "..."))
  (:documentation "..."))

(defmethod initialize-instance :after ((ob prop-info)
                                       &key base-name namespace)
  (setf (slot-value ob 'full-name)
        (prop-name-string (cons base-name namespace) :reverse-p t)))

(defun print-prop-info (pinfo)
  (format t "~%[PROP-NAME-STR] ~S~%" (prop-info-full-name pinfo))
  (format t "~%[PROP-META] ~S~%" (prop-info-meta pinfo)))

(defclass attr-info (prop-info)
  ((array-p
    :type boolean
    :initarg :array-p
    :initform nil
    :reader attr-info-array-p
    :documentation "...")
   (type-key
    :type keyword
    :initarg :type-key
    :initform (error "...")
    :reader attr-info-type-key
    :documentation "..."))
  (:documentation "..."))

(defun get-attr-type (ainfo table)
  (gethash (attr-info-type-key ainfo) table))

(defclass rel-info (prop-info)
  ()
  (:documentation "..."))

(defstruct property
  (info (error "...") :type prop-info :read-only t)
  (data nil))

(defstruct compound
  (properties nil))
