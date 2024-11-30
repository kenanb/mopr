;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (cnode
            (:copier nil))
  "Structure: CNODE

Represents  the readably-printable,  serializable  and content-addressable  core
node content.

Mutability:

The class  design neither prevents, nor  guarantees the ability to  mutate.  The
class allows  both use-cases,  but reusability of  its instances  are inherently
more limited. It is left to the owner of the cnode tree to establish the policy,
Receiver  of the  tree should  follow this  policy, and  if needed,  construct a
deep-copy."

  (payload (error "A cnode cannot be initialized without a payload.")
   :type payload)
  (children (make-array 0 :element-type 'cnode
                          :adjustable t
                          :fill-pointer 0)
   :type (vector cnode)))

(defun cnode-from-node-recursive (inode &aux (onode (make-cnode :payload (cnode-payload cn))))
  (loop for ch across (cnode-children inode)
        for ch-new = (cnode-from-node-recursive ch)
        do (vector-push-extend ch-new (cnode-children onode)))
  onode)

(defun cnode-debug-print-recursive (node &optional (nesting 0))
  (format t "~S - ~S~%" nesting (cnode-payload node))
  (loop for ch across (cnode-children node) do (cnode-debug-print-recursive ch (1+ nesting))))

(defun cnode-debug-print (node)
  (format t "DEBUG PRINTING NODE:~%")
  (cnode-debug-print-recursive node)
  (format t "~%" node))
