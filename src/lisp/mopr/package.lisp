;;;; package.lisp

(uiop:define-package #:mopr
    (:use #:cl)
  (:use-reexport #:mopr-def
                 #:mopr-fun)
  (:export #:with-handle
           #:with-handles
           #:with-handles*))

(defpackage :mopr-val
  (:use #:cl)
  (:export
   #:get-real-type
   #:value-type-p
   #:value-type-dims
   #:value-type-rank
   #:value-type-nof-elt
   #:value-type-elt-type
   #:value-type-real-type
   #:value-type-name
   #:create-generic-value-type-tokens
   #:delete-generic-value-type-tokens
   #:transfer-for-type
   #:get-transfer-for-type-function))

(defpackage :mopr-prim
  (:use #:cl)
  (:export
   #:create-generic-prim-type-tokens
   #:delete-generic-prim-type-tokens))

(defpackage :mopr-sgt
  (:use #:cl)
  (:export
   #:prop-name-string
   #:prop-info
   #:attr-info
   #:rel-info
   #:prop-info-namespace
   #:prop-info-base-name
   #:prop-info-full-name
   #:prop-info-meta
   #:print-prop-info
   #:attr-info-array-p
   #:attr-info-type-key
   #:get-attr-type
   #:data-group
   #:tree-entry
   #:prim-entry
   #:prop-entry
   #:make-data-group
   #:make-tree-entry
   #:make-prim-entry
   #:make-prop-entry
   #:data-group-data
   #:tree-entry-data
   #:prim-entry-data
   #:prop-entry-data
   #:prop-entry-info))

(defpackage :mopr-ns
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
