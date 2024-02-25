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
   #:+value-type-list+
   #:+value-role-list+
   #:get-real-type
   #:transfer-for-type
   #:get-transfer-for-type-function))

(defpackage :mopr-info
  (:use #:cl)
  (:export

   ;; Utilities:
   #:*registry-supported-cases*

   ;; Value types:
   #:value-type
   #:value-type-p
   #:value-type-dims
   #:value-type-rank
   #:value-type-nof-elt
   #:value-type-elt-type
   #:value-type-real-type
   #:value-type-name

   ;; Properties:
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

   ;; Schemas:
   #:schema
   #:schema-name-token
   #:schema-type
   #:schema-family
   #:schema-kind
   #:schema-version
   #:schema-prop-table

   ;; Registry:
   #:*registry*
   #:registry
   #:teardown-entry
   #:make-registry
   #:populate-registry
   #:teardown-registry
   #:with-registry
   #:get-schema
   #:get-value-type-for-attr-info
   #:get-prop-info-for-schema
   #:get-value-type-for-attr
   #:get-element-type-for-attr))

(defpackage :mopr-sgt
  (:use #:cl)
  (:export
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
