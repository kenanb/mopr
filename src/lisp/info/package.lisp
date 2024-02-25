;;;; package.lisp

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
   #:make-registry
   #:populate-registry
   #:teardown-registry
   #:with-registry

   ;; Utilities:
   #:get-schema
   #:get-value-type-for-attr-info
   #:get-prop-info-for-schema
   #:get-value-type-for-attr
   #:get-element-type-for-attr))
