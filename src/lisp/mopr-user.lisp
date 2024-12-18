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

(mopr-ffi:init-core)
(mopr-ffi:init-wrap)
(mopr-gui/yoga-ffi:init-yoga)
(mopr-gui/repr-ffi:init-repr)
