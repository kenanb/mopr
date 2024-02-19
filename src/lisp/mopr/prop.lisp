;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-prop)

(defun prop-name-string (r-prop-name &key reverse-p)
  (format nil "~{~A~^:~}" (if reverse-p
                              (reverse r-prop-name)
                              r-prop-name)))

(defclass prop-info ()
  ((name-rlist
    :type list
    :initarg :name-rlist
    :initform nil
    :reader prop-info-name-rlist
    :documentation "...")
   (name-string
    :type base-string
    :initarg :name-string
    :initform (error "...")
    :reader prop-info-name-string
    :documentation "...")
   (meta
    :type list
    :initarg :meta
    :initform nil
    :reader prop-info-meta
    :documentation "..."))
  (:documentation "..."))

(defun print-prop-info (pinfo)
  (format t "~%[PROP-NAME-STR] ~S~%" (prop-info-name-string pinfo))
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
