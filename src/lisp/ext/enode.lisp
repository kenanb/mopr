;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/enode
  (:import-from :mopr)
  (:use :cl)
  (:export

   ;; ENODE API
   #:enode
   #:enode-parent
   #:enode-children
   #:enode-extensions

   ;; Generic APIs
   #:enode-find-extension
   #:enode-initialize-extension

   ;; ROOT-ENODE
   #:root-enode

   ;; VAR-ENODE
   #:var-enode
   #:var-enode-name-param
   #:var-enode-aux-form-param
   #:var-enode-val-form-param

   ;; EACH-ENODE
   #:each-enode
   #:each-enode-name-params
   #:each-enode-keys-form-param
   #:each-enode-vals-form-param

   ;; IOTA-ENODE
   #:iota-enode
   #:iota-enode-name-param
   #:iota-enode-key-param
   #:iota-enode-end-param

   ;; CALL-ENODE
   #:call-enode
   #:call-enode-aux-form-param
   #:call-enode-body-form-param

   ;; PRIM-CALL-ENODE
   #:prim-call-enode

   ;; PRIM-TYPE-ENODE
   #:prim-type-enode
   #:prim-type-enode-name-param

   ;; PRIM-NS-ENODE
   #:prim-ns-enode
   #:prim-ns-enode-name-param

   ;; PRIM-ATTR-ENODE
   #:prim-attr-enode
   #:prim-attr-enode-name-param
   #:prim-attr-enode-meta-form-param
   #:prim-attr-enode-category-param
   #:prim-attr-enode-type-param
   #:prim-attr-enode-body-form-param

   ;; PRIM-REL-ENODE
   #:prim-rel-enode
   #:prim-rel-enode-name-param
   #:prim-rel-enode-meta-form-param
   #:prim-rel-enode-body-form-param

   ;; PRIM-META-ENODE
   #:prim-meta-enode

   ;; PRIM-ENODE
   #:prim-enode
   #:prim-enode-path-form-param

   ;; TREE-ENODE
   #:tree-enode
   #:tree-enode-body-form-param

   ;; META-ENODE
   #:meta-enode
   #:meta-enode-body-form-param

   ))

(in-package :mopr-ext/enode)

;;
;;; ENODE and Generic Functions
;;

(defclass enode ()
  ((children
    :type (vector enode)
    :initform (make-array 0 :element-type 'enode :adjustable t :fill-pointer 0)
    :accessor enode-children)
   (parent
    :type (or null enode)
    :initarg :parent
    :initform nil
    :accessor enode-parent)
   (extensions
    :type list
    :initarg :extensions
    :initform nil
    :accessor enode-extensions)))

(defgeneric enode-initialize-extension (node ext)
  (:documentation "Populate the extension bound to enode."))

(defmethod initialize-instance :after ((node enode) &key)
  (loop for ext in (enode-extensions node)
        do (enode-initialize-extension node ext)))

(defun enode-find-extension (node typ)
  (loop for ext in (enode-extensions node) when (typep ext typ)
        return ext))

(defclass root-enode (enode)
  ())

(defclass var-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor var-enode-name-param)
   (aux-form-param
    :type list
    :initform nil
    :initarg :aux-form-param
    :accessor var-enode-aux-form-param)
   (val-form-param
    :type list
    :initform nil
    :initarg :val-form-param
    :accessor var-enode-val-form-param)))

(defclass each-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor each-enode-name-param)
   (keys-form-param
    :type list
    :initform nil
    :initarg :keys-form-param
    :accessor each-enode-keys-form-param)
   (vals-form-param
    :type list
    :initform nil
    :initarg :vals-form-param
    :accessor each-enode-vals-form-param)))

(defclass iota-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor iota-enode-name-param)
   (key-param
    :type base-string
    :initarg :key-param
    :accessor iota-enode-key-param)
   (end-param
    :type integer
    :initarg :end-param
    :accessor iota-enode-end-param)))

(defclass call-enode (enode)
  ((aux-form-param
    :type list
    :initarg :aux-form-param
    :accessor call-enode-aux-form-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor call-enode-body-form-param)))

(defclass prim-call-enode (call-enode)
  ())

(defclass prim-type-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-type-enode-name-param)))

(defclass prim-attr-enode (enode)
  ((name-param
    :type (or symbol base-string)
    :initarg :name-param
    :accessor prim-attr-enode-name-param)
   (meta-form-param
    :type list
    :initarg :meta-form-param
    :initform nil
    :accessor prim-attr-enode-meta-form-param)
   (category-param
    :type keyword
    :initarg :category-param
    :accessor prim-attr-enode-category-param)
   (type-param
    :type keyword
    :initarg :type-param
    :accessor prim-attr-enode-type-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-attr-enode-body-form-param)))

(defclass prim-rel-enode (enode)
  ((name-param
    :type (or symbol base-string)
    :initarg :name-param
    :accessor prim-rel-enode-name-param)
   (meta-form-param
    :type list
    :initarg :meta-form-param
    :initform nil
    :accessor prim-rel-enode-meta-form-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-rel-enode-body-form-param)))

(defclass prim-ns-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-ns-enode-name-param)))

(defclass prim-enode (enode)
  ((path-form-param
    :type list
    :initarg :path-form-param
    :accessor prim-enode-path-form-param)))

(defclass tree-enode (enode)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor tree-enode-body-form-param)))

(defclass meta-enode (enode)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor meta-enode-body-form-param)))

(defclass prim-meta-enode (meta-enode)
  ())
