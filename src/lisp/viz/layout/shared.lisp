;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-viz/layout-shared
  (:use :cl)
  (:export
   #:layout-dimension
   #:with-layout-settings))

(in-package :mopr-viz/layout-shared)

(defmacro layout-dimension (ynode accessor)
  (case accessor
    (:left `(mopr-viz/yoga-fun:node-layout-get-left ,ynode))
    (:right `(mopr-viz/yoga-fun:node-layout-get-right ,ynode))
    (:top `(mopr-viz/yoga-fun:node-layout-get-top ,ynode))
    (:bottom `(mopr-viz/yoga-fun:node-layout-get-bottom ,ynode))
    (:width `(mopr-viz/yoga-fun:node-layout-get-width ,ynode))
    (:height `(mopr-viz/yoga-fun:node-layout-get-height ,ynode))))

(defmacro with-layout-settings (&body body)
  `(float-features:with-float-traps-masked (:invalid)
     ,@body))
