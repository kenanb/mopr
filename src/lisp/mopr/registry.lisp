;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-reg)

(defstruct registry
  (value-type-table
   (make-hash-table)
   :type hash-table)
  (isa-schema-table
   (make-hash-table)
   :type hash-table)
  (api-schema-table
   (make-hash-table)
   :type hash-table))

(defvar *registry* nil)

(defun create-registry-tables ()
  (mopr-val:create-generic-value-type-table (registry-value-type-table *registry*))
  (mopr-scm:create-generic-isa-schema-table (registry-isa-schema-table *registry*))
  (mopr-scm:create-generic-api-schema-table (registry-api-schema-table *registry*)))

(defun delete-registry-tables ()
  (mopr-scm:delete-generic-api-schema-table (registry-api-schema-table *registry*))
  (mopr-scm:delete-generic-isa-schema-table (registry-isa-schema-table *registry*))
  (mopr-val:delete-generic-value-type-table (registry-value-type-table *registry*)))

(defmacro with-registry (&body body)
  `(let ((*registry* (make-registry)))
     (prog2
         (create-registry-tables)
         (progn
           ,@body)
       (delete-registry-tables))))

(declaim (inline get-value-type-table
                 get-schema-table))

(defun get-value-type-table ()
  (registry-value-type-table *registry*))

(defun get-schema-table (schema-type)
  (funcall
   (case schema-type
     (:isa #'registry-isa-schema-table)
     (:api #'registry-api-schema-table)
     (otherwise (error "Unknown keyword for schema type!")))
   *registry*))

(defun get-schema (schema-type schema-name)
  (gethash schema-name (get-schema-table schema-type)))

(defun get-value-type-for-attr-info (attr-info)
  (gethash (mopr-scm:attr-info-type-key attr-info)
           (get-value-type-table)))

(defun get-prop-info-for-schema (schema-type schema-name prop-name)
  (gethash prop-name
           (mopr-scm:schema-prop-table
            (get-schema schema-type schema-name))))

(defun get-value-type-for-attr (schema-type schema-name attr-name
                                &aux
                                  (prop-info (get-prop-info-for-schema
                                              schema-type
                                              schema-name
                                              attr-name)))
  (when (typep prop-info 'mopr-scm:attr-info)
    (get-value-type-for-attr-info prop-info)))

(defun get-element-type-for-attr (schema-type schema-name attr-name
                                  &aux
                                    (value-type (get-value-type-for-attr
                                                 schema-type
                                                 schema-name
                                                 attr-name)))
  (when value-type (mopr-val:value-type-elt-type value-type)))
