;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (cnode
            (:copier nil)
            (:constructor make-cnode
                (&key payload &aux (digest (register-payload payload)))))
  "Structure: CNODE

Represents  the readably-printable,  serializable  and content-addressable  core
node content.

Mutability:

The class  design neither prevents, nor  guarantees the ability to  mutate.  The
class allows  both use-cases,  but reusability of  its instances  are inherently
more limited. It is left to the owner of the cnode tree to establish the policy,
Receiver  of the  tree should  follow this  policy, and  if needed,  construct a
deep-copy."

  (digest (error "A CNODE cannot be initialized without a payload code.")
   :type base-string)
  (children (make-array 0 :element-type 'cnode
                          :adjustable t
                          :fill-pointer 0)
   :type (vector cnode)))

(defun cnode-find-payload (node)
  (find-payload (cnode-digest node)))

(defun cnode-from-node-recursive (inode &aux (onode (make-cnode :payload (cnode-find-payload inode))))
  (loop for ch across (cnode-children inode)
        for ch-new = (cnode-from-node-recursive ch)
        do (vector-push-extend ch-new (cnode-children onode)))
  onode)

(defun cnode-debug-print-recursive (node &optional (nesting 0))
  (format t "~S - ~S~%" nesting (cnode-find-payload node))
  (loop for ch across (cnode-children node) do (cnode-debug-print-recursive ch (1+ nesting))))

(defun cnode-debug-print (node)
  (format t "DEBUG PRINTING NODE:~%")
  (cnode-debug-print-recursive node)
  (format t "~%" node))
