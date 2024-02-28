;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-user
  (:use
   #:cl
   #:mopr)
  (:export)

  (:use
   #:mopr-ext/usds))

(in-package #:mopr-user)

(defconstant +configuration+
  `((:prim-callables ,mopr-ext/grid::+prim-callables+)
    (:prim-callables ,mopr-ext/test::+prim-callables+)
    (:data-callables ,mopr-ext/test::+data-callables+)))

(mopr-ffi:init-core)
(mopr-ffi:init-wrap)
