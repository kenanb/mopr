;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (bnode
            (:copier nil)
            (:constructor nil))
  "Structure: BNODE

Represents the base node content.

Mutability:

The class  design neither prevents, nor  guarantees the ability to  mutate.  The
class allows  both use-cases,  but reusability of  its instances  are inherently
more limited. It is left to the owner of the bnode tree to establish the policy,
Receiver  of the  tree should  follow this  policy, and  if needed,  construct a
deep-copy."

  (children (make-array 0 :element-type 'bnode
                          :adjustable t
                          :fill-pointer 0)
   :type (vector bnode)))

(defgeneric bnode-find-payload (node)
  (:documentation "Get payload for bnode."))

(defgeneric bnode-payload-digest (node)
  (:documentation "Get or calculate a digest for bnode payload."))

(defun %calculate-digest (digests)
  (let* ((digests-str (apply #'concatenate 'string digests))
         (octets (ironclad:ascii-string-to-byte-array digests-str))
         (digest (ironclad:digest-sequence :sha1 octets)))
    (ironclad:byte-array-to-hex-string digest)))

(defun bnode-calculate-digest (node)
  (let ((p-digest (bnode-payload-digest node))
        (c-digest (%calculate-digest (loop for ch across (cnode-children node)
                                           collecting (bnode-calculate-digest ch)))))
    (%calculate-digest (list p-digest c-digest))))

(defun bnode-debug-print-recursive (node &optional (nesting 0))
  (format t "~S - ~S~%" nesting (bnode-find-payload node))
  (loop for ch across (bnode-children node) do (bnode-debug-print-recursive ch (1+ nesting))))

(defun bnode-debug-print (node)
  (format t "DEBUG PRINTING NODE:~%")
  (bnode-debug-print-recursive node)
  (format t "~%"))
