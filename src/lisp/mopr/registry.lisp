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

(defun get-attr-type (attr-info)
  (gethash (mopr-scm:attr-info-type-key attr-info) (registry-value-type-table *registry*)))

(defun get-isa-schema (prim-type)
  (gethash prim-type (registry-isa-schema-table *registry*)))

(defun get-prop-info-for-isa-schema (schema-type prop-name)
  (gethash prop-name
           (mopr-scm:schema-prop-table
            (gethash schema-type (registry-isa-schema-table *registry*)))))

(defun get-api-schema (prim-type)
  (gethash prim-type (registry-api-schema-table *registry*)))
