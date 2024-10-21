;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr-shared
  (:import-from :mopr)
  (:import-from :plus-c) ; Depend on system.
  (:import-from :float-features) ; Depend on system.
  (:use :cl)
  (:export
   #:layout-dimension
   #:multiple-set-c-ref
   #:with-layout-settings))

(in-package :mopr-ext/repr-shared)

(defmacro layout-dimension (ynode accessor)
  (case accessor
    (:left `(yoga-fun:node-layout-get-left ,ynode))
    (:right `(yoga-fun:node-layout-get-right ,ynode))
    (:top `(yoga-fun:node-layout-get-top ,ynode))
    (:bottom `(yoga-fun:node-layout-get-bottom ,ynode))
    (:width `(yoga-fun:node-layout-get-width ,ynode))
    (:height `(yoga-fun:node-layout-get-height ,ynode))))

(defmacro multiple-set-c-ref (obj accessor &rest key-value-plist)
  `(progn
     ,@(loop for (k v . rest) on key-value-plist by #'cddr
             collecting `(setf (plus-c:c-ref ,obj ,@accessor ,k) ,v))))

(defmacro with-layout-settings (&body body)
  `(float-features:with-float-traps-masked (:invalid)
     ,@body))
