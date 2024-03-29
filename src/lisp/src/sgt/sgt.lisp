;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(deftype any-timecode ()
  "Type definition for timecode, including the Default time."
  '(or null float integer))

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
            (:constructor make-prop (datum time info-args
                                     &aux
                                       (info (apply #'mopr-info:get-prop-info-for-schema
                                              info-args))
                                       (data (list (if time (cons time datum) datum))))))
  (info (error "...") :type mopr-info:prop-info :read-only t)
  (data nil))

(defconstant +sgt-op-callables+
  '(:make-prop
    #S(mopr-plug:callable :fn make-prop
                          :i (:datum t :time any-timecode :info-args list)
                          :o (:prop prop-entry))

    :make-group
    #S(mopr-plug:callable :fn make-group
                          :i (:prop-list list)
                          :o (:group data-group))))

(defconstant +configuration+
  `((:callables ,+sgt-op-callables+)))
