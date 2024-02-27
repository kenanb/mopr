;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-info)

(declaim (inline get-value-type-table
                 get-schema-table))

(defun get-value-type-table ()
  (entry-table (registry-value-types *registry*)))

(defun get-schema-table (schema-type)
  (entry-table
   (funcall
    (case schema-type
      (:isa #'registry-isa-schemas)
      (:api #'registry-api-schemas)
      (otherwise (error "Unknown keyword for schema type!")))
    *registry*)))

(defun get-schema (schema-type schema-name)
  (gethash schema-name (get-schema-table schema-type)))

(defun get-value-type-for-attr-info (attr-info)
  (gethash (attr-info-type-key attr-info)
           (get-value-type-table)))

(defun get-prop-info-for-schema (schema-type schema-name prop-name)
  (gethash prop-name
           (schema-prop-table (get-schema schema-type schema-name))))

(defun get-value-type-for-attr (schema-type schema-name attr-name
                                &aux
                                  (prop-info (get-prop-info-for-schema
                                              schema-type
                                              schema-name
                                              attr-name)))
  (when (typep prop-info 'attr-info)
    (get-value-type-for-attr-info prop-info)))

(defun get-element-type-for-attr (schema-type schema-name attr-name
                                  &aux
                                    (value-type (get-value-type-for-attr
                                                 schema-type
                                                 schema-name
                                                 attr-name)))
  (when value-type (value-type-elt-type value-type)))
