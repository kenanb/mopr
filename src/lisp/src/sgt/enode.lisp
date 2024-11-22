;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

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
   (components
    :type list
    :initarg :components
    :initform nil
    :accessor enode-components)))

(defun enode-add-components-recursive (node component-classes)
  (loop for cc in component-classes
        unless (member cc (enode-components node) :key #'type-of)
          do (push (make-instance cc) (enode-components node)))
  (loop for ch across (enode-children node)
        do (enode-add-components-recursive ch component-classes)))

(defgeneric enode-initialize-component (node component)
  (:documentation "Populate the component bound to enode."))

(defun enode-initialize-components-recursive (node)
  (loop for co in (enode-components node) do (enode-initialize-component node co))
  (loop for ch across (enode-children node) do (enode-initialize-components-recursive ch)))

(defun enode-find-component (node component-class)
  (loop for co in (enode-components node) when (typep co component-class) return co))

(defun debug-print-recursive (node &optional (nesting 0))
  (format t "~S - ~S~%" nesting node)
  (loop for ch across (enode-children node) do (debug-print-recursive ch (1+ nesting))))

(defun debug-print (node)
  (format t "DEBUG PRINTING NODE: ~S~%" node)
  (debug-print-recursive node)
  (format t "~%" node))

;;
;;; ENODE Categories
;;

(defclass execution-enode (enode)
  ())

(defclass container-enode (enode)
  ())

(defclass directive-enode (enode)
  ())

;;
;;; Concrete ENODE Classes
;;

(defclass root-enode (container-enode)
  ())

(defclass group-enode (container-enode)
  ())

(defclass var-enode (directive-enode)
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

(defclass each-enode (directive-enode)
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

(defclass iota-enode (directive-enode)
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
    :accessor iota-enode-end-param)
   (start-param
    :type (or null integer)
    :initarg :start-param
    :initform nil
    :accessor iota-enode-start-param)
   (step-param
    :type (or null integer)
    :initarg :step-param
    :initform nil
    :accessor iota-enode-step-param)))

(defclass call-enode (directive-enode)
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

;; TODO : Unserializable and unrepresentable until the implementation is revised
;;        to only contain the information needed to generate the prop-info instance.
(defclass prim-schema-prop-enode (execution-enode)
  ((info-param
    :type mopr-info:prop-info
    :initarg :info-param
    :accessor prim-schema-prop-enode-info-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-schema-prop-enode-body-form-param)))

(defclass prim-type-enode (execution-enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-type-enode-name-param)))

(defclass prim-attr-enode (execution-enode)
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

(defclass prim-rel-enode (execution-enode)
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

(defclass prim-ns-enode (container-enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-ns-enode-name-param)))

(defclass prim-enode (execution-enode)
  ((path-form-param
    :type list
    :initarg :path-form-param
    :accessor prim-enode-path-form-param)))

(defclass tree-enode (execution-enode)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor tree-enode-body-form-param)))

(defclass meta-enode (execution-enode)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor meta-enode-body-form-param)))

(defclass prim-meta-enode (meta-enode)
  ())
