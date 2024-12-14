;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(deftype any-timecode ()
  "Type definition for timecode, including the Default time."
  '(or null float integer))

(defun as-timesample (datum time)
  (if time (cons time datum) datum))

;; NOTE: We wrap datum in a list here. That's because even though
;; make-prim-schema-prop-statement supports multiple timecode (or target) assignment,
;; make-schema-prop callable only supports single timesample (or target) assignment.
;; in order to keep call directives less verbose for the majority usecase,
(defun make-schema-prop (datum info-args)
  (as-dnode
   (make-prim-schema-prop-statement
    :info-args-param info-args
    :body-form-param (list datum))))

(defun make-group (data
                   &aux (node (as-dnode (make-group-container))))
  (loop for ch in data do (vector-push-extend ch (dnode-children node)))
  node)

(defconstant +sgt-op-callables+
  '(:as-timesample
    #S(mopr-sgt/plug:callable :fn as-timesample
                              :i (:datum t :time any-timecode)
                              :o (:datum t))

    :make-schema-prop
    #S(mopr-sgt/plug:callable :fn make-schema-prop
                              :i (:datum t :info-args list)
                              :o (:prop dnode))

    :make-group
    #S(mopr-sgt/plug:callable :fn make-group
                              :i (:prop-list list)
                              :o (:group dnode))))

(defconstant +configuration+
  `((:callables ,+sgt-op-callables+)))
