;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-usd/info)

(defun generate-prop-info (prop-name prop-def-h)
  (let (info-type
        info-args)
    (if (zerop (mopr-usd:property-definition-is-attribute-p prop-def-h))
        (setf info-type 'rel-info)
        (mopr-usd:with-handles* ((scalar-token-h :token)
                                 (this-vtn-h :value-type-name)
                                 (scalar-vtn-h :value-type-name))
          (setf info-type 'attr-info)
          (mopr-usd:property-definition-attribute-get-type-name this-vtn-h prop-def-h)
          (let ((array-p (not (zerop (mopr-usd:value-type-name-is-array-p this-vtn-h)))))
            (setf (getf info-args :array-p) array-p)
            (if array-p
                (progn
                  (mopr-usd:value-type-name-get-scalar-type scalar-vtn-h this-vtn-h)
                  (mopr-usd:value-type-name-get-as-token scalar-token-h scalar-vtn-h))
                (mopr-usd:value-type-name-get-as-token scalar-token-h this-vtn-h))
            (setf (getf info-args :type-key)
                  (intern (mopr-usd:token-cstr scalar-token-h) :keyword)))))
    (apply #'make-instance info-type
           :base-name prop-name
           :variability (mopr-usd:property-definition-get-variability prop-def-h)
           info-args)))

(defun create-property-info-table (prim-token-h schema-type
                                   &aux
                                     prim-def-fn
                                     (table (make-hash-table)))
  (setf prim-def-fn
        (case schema-type
          (:api #'mopr-usd:prim-definition-ctor-api)
          (:isa #'mopr-usd:prim-definition-ctor-isa)
          (otherwise (error "Unknown keyword for schema type!"))))

  (mopr-usd:with-handles* ((prim-def-h :prim-definition)
                           (prop-token-h :token)
                           (prop-def-h :property-definition))
    (funcall prim-def-fn prim-def-h prim-token-h)
    (when (zerop (mopr-usd:prim-definition-is-empty-p prim-def-h))
      (loop for i below (mopr-usd:prim-definition-get-property-count prim-def-h)
            do (progn
                 (mopr-usd:prim-definition-get-property-name prop-token-h prim-def-h i)
                 (mopr-usd:prim-definition-get-property prop-def-h prim-def-h prop-token-h)
                 (let* ((prop-name (mopr-usd:token-cstr prop-token-h))
                        (info (generate-prop-info prop-name prop-def-h)))
                   (write-mapping-with-case table
                                            (intern prop-name :keyword)
                                            info))))))
  table)

(defstruct (schema
            (:constructor make-schema
                (schema-name
                 family
                 type
                 kind
                 version
                 &aux
                   (name-token
                    (let ((prim-token-h (mopr-usd:create-token)))
                      (mopr-usd:token-ctor-cstr
                       prim-token-h
                       schema-name)
                      prim-token-h))
                   (prop-table (create-property-info-table
                                name-token type)))))
  "..."
  (name-token
   (error "SCHEMA should have a NAME-TOKEN.")
   :type mopr-usd:mopr-token-h)
  (family
   (error "SCHEMA should have a FAMILY.")
   :type keyword)
  (type
   (error "SCHEMA should have a TYPE.")
   :type (member :isa :api))
  (kind
   (error "SCHEMA should have a KIND.")
   :type fixnum)
  (version
   (error "SCHEMA should have a VERSION.")
   :type fixnum)
  (prop-table
   (error "SCHEMA should have a PROP-TABLE.")
   :type hash-table))

(defmethod teardown-entry ((val schema)
                           &aux (tok (schema-name-token val)))
  ;; (format t "DELETING SCHEMA: ~A~%" val)
  (mopr-usd:delete-token tok)
  (autowrap:invalidate tok))
