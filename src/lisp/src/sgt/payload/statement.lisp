;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defclass statement (payload)
  ())

;; TODO : Unserializable and unrepresentable until the implementation is revised
;;        to only contain the information needed to generate the prop-info instance.
(defclass prim-schema-prop-statement (statement)
  ((info-param
    :type mopr-info:prop-info
    :initarg :info-param
    :accessor prim-schema-prop-statement-info-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-schema-prop-statement-body-form-param)))

(defclass prim-type-statement (statement)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-type-statement-name-param)))

(defclass prim-attr-statement (statement)
  ((name-param
    :type (or symbol base-string)
    :initarg :name-param
    :accessor prim-attr-statement-name-param)
   (meta-form-param
    :type list
    :initarg :meta-form-param
    :initform nil
    :accessor prim-attr-statement-meta-form-param)
   (category-param
    :type keyword
    :initarg :category-param
    :accessor prim-attr-statement-category-param)
   (type-param
    :type keyword
    :initarg :type-param
    :accessor prim-attr-statement-type-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-attr-statement-body-form-param)))

(defclass prim-rel-statement (statement)
  ((name-param
    :type (or symbol base-string)
    :initarg :name-param
    :accessor prim-rel-statement-name-param)
   (meta-form-param
    :type list
    :initarg :meta-form-param
    :initform nil
    :accessor prim-rel-statement-meta-form-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-rel-statement-body-form-param)))

(defclass prim-statement (statement)
  ((path-form-param
    :type list
    :initarg :path-form-param
    :accessor prim-statement-path-form-param)))

(defclass tree-statement (statement)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor tree-statement-body-form-param)))

(defclass meta-statement (statement)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor meta-statement-body-form-param)))

(defclass prim-meta-statement (meta-statement)
  ())
