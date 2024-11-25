;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defclass container (payload)
  ())

(defclass root-container (container)
  ())

(defclass group-container (container)
  ())

(defclass prim-ns-container (container)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-ns-container-name-param)))
