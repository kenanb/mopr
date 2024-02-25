;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-reg)

(defclass registry-entry-bundle ()
  ((array
    :reader entry-array
    :type vector)
   (table
    :reader entry-table
    :initform (make-hash-table)
    :type hash-table))
  (:documentation "..."))

(defmethod initialize-instance :after ((ob registry-entry-bundle)
                                       &key (default-array-size 16))
  (setf (slot-value ob 'array)
        (make-array
         default-array-size
         :adjustable t
         :fill-pointer 0
         :element-type 'symbol
         :initial-element nil)))

(defclass schema-entry-bundle (registry-entry-bundle) ())

(defclass isa-schemas (schema-entry-bundle)
  ()
  (:default-initargs :default-array-size 128))

(defclass api-schemas (schema-entry-bundle)
  ()
  (:default-initargs :default-array-size 64))

(defclass value-types (registry-entry-bundle)
  ()
  (:default-initargs :default-array-size 128))

(defgeneric populate-entries (ob)
  (:method ((ob t)) (error "Couldn't find specialized populator!"))
  (:method ((ob value-types))
    (mopr-val:create-generic-value-types
     (entry-table ob)
     (entry-array ob)))
  (:method ((ob isa-schemas))
    (mopr-scm:create-generic-schemas
     :isa
     (entry-table ob)
     (entry-array ob)))
  (:method ((ob api-schemas))
    (mopr-scm:create-generic-schemas
     :api
     (entry-table ob)
     (entry-array ob))))

(defgeneric teardown-entry (ob)
  (:method ((ob t)) (error "Couldn't find specialized teardown!")))

(defun teardown-entries (bundle)
  (loop for s across (entry-array bundle)
        do (teardown-entry (gethash s (entry-table bundle)))))

(defstruct registry
  (value-types (make-instance 'value-types) :type value-types)
  (isa-schemas (make-instance 'isa-schemas) :type isa-schemas)
  (api-schemas (make-instance 'api-schemas) :type api-schemas))

(defvar *registry* nil)

(defun populate-registry ()
  (populate-entries (registry-api-schemas *registry*))
  (populate-entries (registry-isa-schemas *registry*))
  (populate-entries (registry-value-types *registry*)))

(defun teardown-registry ()
  (teardown-entries (registry-api-schemas *registry*))
  (teardown-entries (registry-isa-schemas *registry*))
  (teardown-entries (registry-value-types *registry*)))

(defmacro with-registry (&body body)
  `(let ((*registry* (make-registry)))
     (prog2
         (populate-registry)
         (progn
           ,@body)
       (teardown-registry))))

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
