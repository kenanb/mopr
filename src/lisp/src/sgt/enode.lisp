;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (enode-core
            (:copier nil))
  "Structure: ENODE-CORE

Represents  the readably-printable,  serializable  and content-addressable  core
enode content.

Mutability:

The class  design neither prevents, nor  guarantees the ability to  mutate.  The
class allows  both use-cases,  but reusability of  its instances  are inherently
more limited. It is left to the owner of the enode tree to establish the policy,
Receiver  of the  tree should  follow this  policy, and  if needed,  construct a
deep-copy."

  (payload (error "An enode cannot be initialized without a payload.")
   :type payload)
  (children (make-array 0 :element-type 'enode-core
                          :adjustable t
                          :fill-pointer 0)
   :type (vector enode-core)))

(defmethod print-enode (node stream)
  (print-unreadable-object (node stream :type t :identity t)
    (princ (type-of (enode-payload node)) stream)))

(defstruct (enode
            (:include enode-core)
            (:copier nil)
            (:print-object print-enode))
  (parent nil
   :type (or null enode))
  (components nil
   :type list))

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
  (format t "DEBUG PRINTING NODE:~% ~S~%" node)
  (debug-print-recursive node)
  (format t "~%" node))
