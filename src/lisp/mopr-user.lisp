;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-user
  (:use
   #:cl)
  (:export))

(in-package #:mopr-user)

(defconstant +configuration+
  `((:callables ,mopr-ext/grid::+callables+)
    (:callables ,mopr-ext/test::+callables+)))

(mopr-usd/ffi:init-core)
(mopr-usd/ffi:init-wrap)
(mopr-viz/yoga-ffi:init-yoga)
(mopr-viz/repr-ffi:init-repr)
