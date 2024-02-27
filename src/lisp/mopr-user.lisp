;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-user
  (:use
   #:cl
   #:mopr)
  (:export)

  (:use
   #:mopr-plug/usds))

(in-package #:mopr-user)

(mopr-ffi:init-core)
(mopr-ffi:init-wrap)
