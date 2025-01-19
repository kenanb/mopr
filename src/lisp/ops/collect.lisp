;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :mopr-ops)

(defgeneric get-info-for-component (co))

(defun collect-info-for-components
    (n component-classes
     &aux (c-id (mopr-sgt:enode-find-component n 'node-identifier)))
  (unless c-id (error "NODE-IDENTIFIER component is missing."))
  (let ((id-node (list "id-node" (node-identifier-id c-id))))
    (append
     (mapcar (lambda (x) (cons id-node x))
             (loop for cc in component-classes
                   for co = (mopr-sgt:enode-find-component n cc)
                   if co
                     append (get-info-for-component co)
                   else
                     do (error "Component missing: ~A" cc)))
     (loop for c across (mopr-sgt:enode-children n)
           appending (collect-info-for-components c component-classes)))))
