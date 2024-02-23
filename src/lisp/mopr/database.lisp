;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-db)

(defstruct database
  (value-type-table
   (make-hash-table)
   :type hash-table)
  (isa-schema-table
   (make-hash-table)
   :type hash-table)
  (api-schema-table
   (make-hash-table)
   :type hash-table))

(defvar *database* nil)

(defun create-database-tables ()
  (mopr-val:create-generic-value-type-table (database-value-type-table *database*))
  (mopr-scm:create-generic-isa-schema-table (database-isa-schema-table *database*))
  (mopr-scm:create-generic-api-schema-table (database-api-schema-table *database*)))

(defun delete-database-tables ()
  (mopr-scm:delete-generic-api-schema-table (database-api-schema-table *database*))
  (mopr-scm:delete-generic-isa-schema-table (database-isa-schema-table *database*))
  (mopr-val:delete-generic-value-type-table (database-value-type-table *database*)))

(defun get-attr-type (attr-info)
  (gethash (mopr-scm:attr-info-type-key attr-info) (database-value-type-table *database*)))

(defun get-isa-schema (prim-type)
  (gethash prim-type (database-isa-schema-table *database*)))

(defun get-prop-info-for-isa-schema (schema-type prop-name)
  (gethash prop-name
           (mopr-scm:schema-prop-table
            (gethash schema-type (database-isa-schema-table *database*)))))

(defun get-api-schema (prim-type)
  (gethash prim-type (database-api-schema-table *database*)))
