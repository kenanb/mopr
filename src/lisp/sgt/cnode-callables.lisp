;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(deftype any-timecode ()
  "Type definition for timecode, including the Default time."
  '(or null float integer))

(defun make-prop (datum time info-args)
  (as-dnode
   (make-prim-schema-prop-statement
    :info-param (apply #'mopr-info:get-prop-info-for-schema info-args)
    :body-form-param (list (if time (cons time datum) datum)))))

(defun make-group (data
                   &aux (node (as-dnode (make-group-container))))
  (loop for ch in data do (vector-push-extend ch (dnode-children node)))
  node)

(defconstant +sgt-op-callables+
  '(:make-prop
    #S(mopr-plug:callable :fn make-prop
                          :i (:datum t :time any-timecode :info-args list)
                          :o (:prop dnode))

    :make-group
    #S(mopr-plug:callable :fn make-group
                          :i (:prop-list list)
                          :o (:group dnode))))

(defconstant +configuration+
  `((:callables ,+sgt-op-callables+)))
