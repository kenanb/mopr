;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :mopr-ops)

(defvar *node-identifier-id-counter*)

(defclass node-identifier ()
  ((id
    :type (unsigned-byte 32)
    ;; Zero value is reserved for "no selection", so INITFORM will INCF.
    :initform (incf *node-identifier-id-counter*)
    :reader node-identifier-id)))

(defmethod mopr-sgt:enode-init-component ((payload mopr-sgt:payload)
                                          node
                                          (component node-identifier))
  nil)

(defmethod mopr-sgt:enode-term-component ((payload mopr-sgt:payload)
                                          node
                                          (component node-identifier))
  nil)

(defmethod mopr-sgt:enode-procedure-create-component-unchecked (pr (cc (eql 'node-identifier)))
  ;; Zero value is reserved for "no selection".
  (let ((*node-identifier-id-counter* 0))
    (call-next-method)))

(defun find-enode-by-id (n id &aux (rn (mopr-sgt:enode-find-component n 'node-identifier)))
  (if (eql (node-identifier-id rn) id) n
      (loop for c across (mopr-sgt:enode-children n)
            for x = (find-enode-by-id c id) if x return x)))
