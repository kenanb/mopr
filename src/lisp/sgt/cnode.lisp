;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (cnode
            (:include bnode)
            (:copier nil)
            (:constructor)
            (:constructor as-cnode
                (payload &aux (digest (register-payload-to-bound-header payload)))))
  "Structure: CNODE

Represents  the readably-printable,  serializable  and content-addressable compact
node content.

Payload is stored in the bound header, in a content-addressed fashion. This is a
compact representation that's suitable for both storage (including history), as
well as more granular change tracking."

  (digest (error "A CNODE cannot be initialized without a payload code.")
   :type base-string))

(defgeneric bnode-find-payload (node)
  (:documentation "Get payload for cnode."))

(defmethod bnode-find-payload ((node cnode))
  (find-payload-in-bound-header (cnode-digest node)))

(defun cnode-from-node-recursive (inode &aux (onode (as-cnode (bnode-find-payload inode))))
  (loop for ch across (cnode-children inode)
        for ch-new = (cnode-from-node-recursive ch)
        do (vector-push-extend ch-new (cnode-children onode)))
  onode)
