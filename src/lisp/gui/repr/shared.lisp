;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-viz/repr-shared
  (:use :cl)
  (:export
   #:multiple-set-c-ref))

(in-package :mopr-viz/repr-shared)

(defmacro multiple-set-c-ref (obj accessor &rest key-value-plist)
  `(progn
     ,@(loop for (k v . rest) on key-value-plist by #'cddr
             collecting `(setf (plus-c:c-ref ,obj ,@accessor ,k) ,v))))
