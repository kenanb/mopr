;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defclass enode ()
  ((payload
    :initarg :payload
    :type payload
    :initform (error "An enode cannot be initialized without a payload.")
    :accessor enode-payload)
   (components
    :type list
    :initarg :components
    :initform nil
    :accessor enode-components)
   (children
    :type (vector enode)
    :initform (make-array 0 :element-type 'enode :adjustable t :fill-pointer 0)
    :accessor enode-children)
   (parent
    :type (or null enode)
    :initarg :parent
    :initform nil
    :accessor enode-parent)))

(defun enode-add-components-recursive (node component-classes)
  (loop for cc in component-classes
        unless (member cc (enode-components node) :key #'type-of)
          do (push (make-instance cc) (enode-components node)))
  (loop for ch across (enode-children node)
        do (enode-add-components-recursive ch component-classes)))

(defgeneric enode-initialize-component (payload node component)
  (:documentation "Populate the component bound to enode."))

(defun enode-initialize-components-recursive (node)
  (loop for co in (enode-components node) do (enode-initialize-component (enode-payload node) node co))
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
