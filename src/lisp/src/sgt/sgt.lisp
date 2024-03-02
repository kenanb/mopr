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
            (:constructor make-prop (schema-type schema attr datum
                                     &optional (time nil)
                                     &aux
                                       (info (mopr-info:get-prop-info-for-schema
                                              schema-type schema attr))
                                       (data (list (if time (cons time datum) datum))))))
  (info (error "...") :type mopr-info:prop-info :read-only t)
  (data nil))

(defconstant +sgt-op-callables+
  '(:make-prop
    #S(mopr-plug:callable :fn make-prop
                          :i (:type :schema :attr :data :time)
                          :o (:attr))

    :make-group
    #S(mopr-plug:callable :fn make-group
                          :i (:prop-list)
                          :o (:group))))

(defconstant +configuration+
  `((:generic-callables ,+sgt-op-callables+)))
