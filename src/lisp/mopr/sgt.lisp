;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct data-group
  (data nil))

(defstruct tree-entry
  (data nil))

(defstruct prim-entry
  (data nil))

(defstruct prop-entry
  (info (error "...") :type mopr-info:prop-info :read-only t)
  (data nil))
