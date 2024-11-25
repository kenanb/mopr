;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defclass directive (payload)
  ())

(defclass var-directive (directive)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor var-directive-name-param)
   (aux-form-param
    :type list
    :initform nil
    :initarg :aux-form-param
    :accessor var-directive-aux-form-param)
   (val-form-param
    :type list
    :initform nil
    :initarg :val-form-param
    :accessor var-directive-val-form-param)))

(defclass each-directive (directive)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor each-directive-name-param)
   (keys-form-param
    :type list
    :initform nil
    :initarg :keys-form-param
    :accessor each-directive-keys-form-param)
   (vals-form-param
    :type list
    :initform nil
    :initarg :vals-form-param
    :accessor each-directive-vals-form-param)))

(defclass iota-directive (directive)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor iota-directive-name-param)
   (key-param
    :type base-string
    :initarg :key-param
    :accessor iota-directive-key-param)
   (end-param
    :type integer
    :initarg :end-param
    :accessor iota-directive-end-param)
   (start-param
    :type (or null integer)
    :initarg :start-param
    :initform nil
    :accessor iota-directive-start-param)
   (step-param
    :type (or null integer)
    :initarg :step-param
    :initform nil
    :accessor iota-directive-step-param)))

(defclass call-directive (directive)
  ((aux-form-param
    :type list
    :initarg :aux-form-param
    :accessor call-directive-aux-form-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor call-directive-body-form-param)))

(defclass prim-call-directive (call-directive)
  ())
