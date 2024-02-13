;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :usds
  (:use #:cl #:cffi)
  (:export
   #:*usds-ns-package*
   #:unknown-form-error
   #:handle-prim-form
   #:handle-meta-form
   #:handle-tree-form
   #:handle-data-form
   #:with-usds-tables
   #:write-to-layer))

(defpackage :usds-cn
  (:use #:cl)
  (:export
   #:*prim-type-table*
   #:*prop-type-table*
   #:create-generic-prim-type-tokens
   #:delete-generic-prim-type-tokens))

(defpackage :usds-ns
  (:use)
  (:export
   ;; Generic prim types:  Standard reader case forms:
   #:|PointInstancer|      #:PointInstancer
   #:|Camera|              #:Camera
   #:|Xform|               #:Xform
   #:|Mesh|                #:Mesh
   #:|NurbsPatch|          #:NurbsPatch
   #:|BasisCurves|         #:BasisCurves
   #:|NurbsCurves|         #:NurbsCurves
   #:|Points|              #:Points
   #:|Capsule|             #:Capsule
   #:|Cone|                #:Cone
   #:|Cube|                #:Cube
   #:|Cylinder|            #:Cylinder
   #:|Sphere|              #:Sphere))
