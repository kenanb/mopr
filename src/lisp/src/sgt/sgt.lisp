;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(deftype any-timecode ()
  "Type definition for timecode, including the Default time."
  '(or null float integer))

(defun make-prop (datum time info-args)
  (make-instance 'prim-schema-prop-enode
                 :info-param (apply #'mopr-info:get-prop-info-for-schema info-args)
                 :body-form-param (list (if time (cons time datum) datum))))

(defun make-group (data
                   &aux (group-node (make-instance 'group-enode)))
  (loop for p in data do (vector-push-extend p (enode-children group-node)))
  group-node)

(defconstant +sgt-op-callables+
  '(:make-prop
    #S(mopr-plug:callable :fn make-prop
                          :i (:datum t :time any-timecode :info-args list)
                          :o (:prop prim-schema-prop-enode))

    :make-group
    #S(mopr-plug:callable :fn make-group
                          :i (:prop-list list)
                          :o (:group group-enode))))

(defconstant +configuration+
  `((:callables ,+sgt-op-callables+)))
