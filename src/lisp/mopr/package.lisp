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
   #:create-generic-value-type-table
   #:delete-generic-value-type-table
   #:transfer-for-type
   #:get-transfer-for-type-function))

(defpackage :mopr-scm
  (:use #:cl)
  (:export

   ;; Propery information:
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

   ;; Schema class:
   #:schema
   #:make-schema
   #:schema-name-token
   #:schema-schema-type
   #:schema-prop-table

   ;; Schema tables:
   #:create-generic-api-schema-table
   #:delete-generic-api-schema-table
   #:create-generic-isa-schema-table
   #:delete-generic-isa-schema-table))

(defpackage :mopr-reg
  (:use #:cl)
  (:export
   #:*registry*
   #:registry
   #:make-registry
   #:create-registry-tables
   #:delete-registry-tables
   #:with-registry
   #:get-schema
   #:get-value-type-for-attr-info
   #:get-prop-info-for-schema
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
