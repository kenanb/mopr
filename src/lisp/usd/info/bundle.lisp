;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-info)

(defconstant +ignored-schema-kinds+
  (list
   mopr-usd:+mopr-schema-kind-invalid+
   mopr-usd:+mopr-schema-kind-abstract-base+
   mopr-usd:+mopr-schema-kind-abstract-typed+
   mopr-usd:+mopr-schema-kind-non-applied-api+))

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

(declaim (inline add-entry))

(defun add-entry (ob sym val
                  &aux
                    (array (entry-array ob))
                    (table (entry-table ob)))
  (vector-push-extend sym array)
  (write-mapping-with-case table sym val))

(defgeneric populate-entries (ob)
  (:method ((ob t)) (error "Couldn't find specialized populator!")))

(declaim (inline schema-valid-p
                 get-schema-family
                 get-schema-id
                 create-schema))

(defun schema-valid-p (schema-info-h)
  (and (zerop (mopr-usd:schema-info-is-empty-p schema-info-h))
       (not (member (mopr-usd:schema-info-get-kind schema-info-h)
                    +ignored-schema-kinds+))))

(defun get-schema-family (schema-info-h)
  (mopr-usd:with-handle (token-h :token)
    (mopr-usd:schema-info-get-family token-h schema-info-h)
    (intern (mopr-usd:token-cstr token-h) :keyword)))

(defun get-schema-id (schema-info-h)
  (mopr-usd:with-handle (token-h :token)
    (mopr-usd:schema-info-get-identifier token-h schema-info-h)
    (intern (mopr-usd:token-cstr token-h) :keyword)))

(defun create-schema (id schema-type schema-info-h)
  (make-schema
   (symbol-name id)
   (get-schema-family schema-info-h)
   schema-type
   (mopr-usd:schema-info-get-kind schema-info-h)
   (mopr-usd:schema-info-get-version schema-info-h)))

(defun create-generic-schemas (bundle type-set-h schema-type)
  (mopr-usd:with-handle (schema-info-h :schema-info)
    (loop for i below (mopr-usd:schema-type-set-get-type-count type-set-h)
          do (mopr-usd:schema-type-set-get-schema-info schema-info-h type-set-h i)
          when (schema-valid-p schema-info-h)
            do (let ((id (get-schema-id schema-info-h)))
                 (add-entry bundle id (create-schema id schema-type schema-info-h))))))

(defmethod populate-entries ((ob isa-schemas))
  (mopr-usd:with-handle (type-set-h :schema-type-set)
    (mopr-usd:schema-type-set-ctor-isa-derived type-set-h)
    (create-generic-schemas ob type-set-h :isa)))

(defmethod populate-entries ((ob api-schemas))
  (mopr-usd:with-handle (type-set-h :schema-type-set)
    (mopr-usd:schema-type-set-ctor-api-derived type-set-h)
    (create-generic-schemas ob type-set-h :api)))

(defmethod populate-entries ((ob value-types))
  (loop for sym in (mapcar #'car (append mopr-val:+value-type-list+
                                         mopr-val:+value-role-list+))
        do (add-entry ob sym (make-value-type sym))))

(defun teardown-entries (bundle)
  (loop for s across (entry-array bundle)
        do (teardown-entry (gethash s (entry-table bundle)))))
