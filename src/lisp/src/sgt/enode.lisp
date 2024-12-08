;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defmethod print-enode (node stream)
  (print-unreadable-object (node stream :type t :identity t)
    (princ (type-of (bnode-find-payload node)) stream)))

(defstruct (enode
            (:include cnode)
            (:copier nil)
            (:constructor make-enode
                (&key payload &aux (digest (register-payload-to-bound-header payload))))
            (:print-object print-enode))
  (parent nil
   :type (or null enode))
  (components nil
   :type list))

(defun enode-from-node-recursive (inode &aux (onode (make-enode :payload (bnode-find-payload inode))))
  (loop for ch across (cnode-children inode)
        for ch-new = (enode-from-node-recursive ch)
        do (vector-push-extend ch-new (cnode-children onode))
        do (setf (enode-parent ch-new) onode))
  onode)

(defun enode-add-components-recursive (node component-classes)
  (loop for cc in component-classes
        unless (member cc (enode-components node) :key #'type-of)
          do (push (make-instance cc) (enode-components node)))
  (loop for ch across (enode-children node)
        do (enode-add-components-recursive ch component-classes)))

(defgeneric enode-initialize-component (payload node component)
  (:documentation "Populate the component bound to enode."))

(defun enode-initialize-components-recursive (node &aux (p (bnode-find-payload node)))
  (loop for co in (enode-components node) do (enode-initialize-component p node co))
  (loop for ch across (enode-children node) do (enode-initialize-components-recursive ch)))

(defun enode-find-component (node component-class)
  (loop for co in (enode-components node) when (typep co component-class) return co))
