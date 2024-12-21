;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defmethod print-enode (node stream)
  (print-unreadable-object (node stream :type t :identity t)
    (princ (type-of (bnode-find-payload node)) stream)))

(defstruct (enode
            (:include cnode)
            (:copier nil)
            (:constructor)
            (:constructor as-enode
                (payload &aux (digest (register-payload-to-bound-header payload))))
            (:print-object print-enode))
  "Structure: ENODE

Represents the extended compact node content, currently used to handle editor
interaction."

  (parent nil
   :type (or null enode))
  (components nil
   :type list))

(defun enode-from-node-recursive (inode &aux (onode (as-enode (bnode-find-payload inode))))
  (loop for ch across (cnode-children inode)
        for ch-new = (enode-from-node-recursive ch)
        do (vector-push-extend ch-new (cnode-children onode))
        do (setf (enode-parent ch-new) onode))
  onode)

(defun enode-find-component (node component-class)
  (loop for co in (enode-components node) when (typep co component-class) return co))

(defun enode-add-components-recursive (node component-classes)
  (loop for cc in component-classes
        for co = (enode-find-component node cc)
        do (if co
               (error "Found existing component for given class!")
               (push (make-instance cc) (enode-components node))))
  (loop for ch across (enode-children node)
        do (enode-add-components-recursive ch component-classes)))

(defgeneric enode-initialize-component (payload node component)
  (:documentation "Populate the component bound to enode."))

(defun enode-initialize-components-recursive (node component-classes
                                              &aux (p (bnode-find-payload node)))
  (loop for cc in component-classes
        for co = (enode-find-component node cc)
        do (if co
               (enode-initialize-component p node co)
               (error "Component to initialize is missing!")))
  (loop for ch across (enode-children node)
        do (enode-initialize-components-recursive ch component-classes)))
