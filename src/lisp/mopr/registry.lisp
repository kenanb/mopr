;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-reg)

(defconstant +ignored-schema-kinds+
  (list
   mopr:+mopr-schema-kind-invalid+
   mopr:+mopr-schema-kind-abstract-base+
   mopr:+mopr-schema-kind-abstract-typed+
   mopr:+mopr-schema-kind-non-applied-api+))

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

(defun add-entry (ob n-sym val
                  &aux
                    (array (entry-array ob))
                    (table (entry-table ob))
                    (u-sym (alexandria:format-symbol "KEYWORD" "~:@(~A~)" n-sym)))
  (vector-push-extend n-sym array)
  (setf (gethash n-sym table) val
        (gethash u-sym table) val))

(defgeneric populate-entries (ob)
  (:method ((ob t)) (error "Couldn't find specialized populator!")))

(defun create-generic-schemas (bundle type-set-h schema-type)
  (mopr:with-handles* ((schema-info-h :schema-info)
                       (family-token-h :token)
                       (id-token-h :token))
    (loop for i below (mopr:schema-type-set-get-type-count type-set-h)
          do (mopr:schema-type-set-get-schema-info schema-info-h type-set-h i)
          when (and (zerop (mopr:schema-info-is-empty-p schema-info-h))
                    (not (member (mopr:schema-info-get-kind schema-info-h)
                                 +ignored-schema-kinds+)))
            do (progn
                 (mopr:schema-info-get-family family-token-h schema-info-h)
                 (mopr:schema-info-get-identifier id-token-h schema-info-h)
                 (let ((id (mopr:token-cstr id-token-h)))
                   (add-entry
                    bundle
                    (alexandria:format-symbol "KEYWORD" "~A" id)
                    (mopr-scm:make-schema id
                                          (alexandria:format-symbol
                                           "KEYWORD" "~A"
                                           (mopr:token-cstr family-token-h))
                                          schema-type
                                          (mopr:schema-info-get-kind schema-info-h)
                                          (mopr:schema-info-get-version schema-info-h)))))
          end)))

(defmethod populate-entries ((ob isa-schemas))
  (mopr:with-handle (type-set-h :schema-type-set)
    (mopr:schema-type-set-ctor-isa-derived type-set-h)
    (create-generic-schemas ob type-set-h :isa)))

(defmethod populate-entries ((ob api-schemas))
  (mopr:with-handle (type-set-h :schema-type-set)
    (mopr:schema-type-set-ctor-api-derived type-set-h)
    (create-generic-schemas ob type-set-h :api)))

(defmethod populate-entries ((ob value-types))
  (loop for n-sym in (mapcar #'car (append mopr-val:+value-type-list+
                                           mopr-val:+value-role-list+))
        do (add-entry ob n-sym (mopr-val:make-value-type n-sym))))

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
