;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (data-group
            (:constructor make-data-group)
            (:constructor make-group (data)))
  (data nil))

(defstruct tree-entry
  (data nil))

(defstruct prim-entry
  (data nil))

(defstruct (prop-entry
            (:constructor make-prop-entry)
            (:constructor make-prop (info-args datum
                                     &optional (time nil)
                                     &aux
                                       (info (apply #'mopr-info:get-prop-info-for-schema
                                              info-args))
                                       (data (list (if time (cons time datum) datum))))))
  (info (error "...") :type mopr-info:prop-info :read-only t)
  (data nil))

(defconstant +sgt-op-callables+
  '(:make-prop
    #S(mopr-plug:callable :fn make-prop
                          :i (:info-args :data :time)
                          :o (:attr))

    :make-group
    #S(mopr-plug:callable :fn make-group
                          :i (:prop-list)
                          :o (:group))))

(defconstant +configuration+
  `((:generic-callables ,+sgt-op-callables+)))
