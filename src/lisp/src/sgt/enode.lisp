;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defmethod print-enode (node stream)
  (print-unreadable-object (node stream :type t :identity t)
    (princ (type-of (enode-payload node)) stream)))

(defstruct (enode
            (:include cnode)
            (:copier nil)
            (:print-object print-enode))
  (parent nil
   :type (or null enode))
  (components nil
   :type list))

(defun enode-from-cnode-recursive (cn &aux (en (make-enode :payload (cnode-payload cn))))
  (loop for ch across (cnode-children cn)
        for ch-ext = (enode-from-cnode-recursive ch)
        do (progn
             (vector-push-extend ch-ext (enode-children en))
             (setf (enode-parent ch-ext) en)))
  en)

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
