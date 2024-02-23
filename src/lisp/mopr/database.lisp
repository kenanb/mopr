;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-db)

(defvar *value-type-table* nil)
(defvar *isa-schema-table* nil)
(defvar *api-schema-table* nil)

(defun get-attr-type (attr-info)
  (gethash (mopr-scm:attr-info-type-key attr-info) *value-type-table*))

(defun get-isa-schema (prim-type)
  (gethash prim-type *isa-schema-table*))

(defun get-prop-info-for-isa-schema (schema-type prop-name)
  (gethash prop-name
           (mopr-scm:schema-prop-table
            (gethash schema-type *isa-schema-table*))))

(defun get-api-schema (prim-type)
  (gethash prim-type *api-schema-table*))
