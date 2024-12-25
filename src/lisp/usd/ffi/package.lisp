;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(push :mopr-usd/ffi *features*)

(defpackage #:mopr-usd/ffi
  (:use)
  (:export #:init-core
           #:init-wrap))

(defpackage #:mopr-usd/def
  (:use))

(defpackage #:mopr-usd/fun
  (:use))
