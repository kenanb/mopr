;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-scm)

(defconstant +ignored-schema-kinds+
  (list
   mopr:+mopr-schema-kind-invalid+
   mopr:+mopr-schema-kind-abstract-base+
   mopr:+mopr-schema-kind-abstract-typed+
   mopr:+mopr-schema-kind-non-applied-api+))

(defun generate-prop-info (prop-name prop-def-h)
  (let (info-type
        info-args)
    (if (zerop (mopr:property-definition-is-attribute-p prop-def-h))
        (setf info-type 'mopr-scm:rel-info)
        (mopr:with-handles*  ((scalar-token-h :token)
                              (this-vtn-h :value-type-name)
                              (scalar-vtn-h :value-type-name))
          (setf info-type 'mopr-scm:attr-info)
          (mopr:property-definition-attribute-get-type-name this-vtn-h prop-def-h)
          (let ((array-p (not (zerop (mopr:value-type-name-is-array-p this-vtn-h)))))
            (setf (getf info-args :array-p) array-p)
            (if array-p
                (progn
                  (mopr:value-type-name-get-scalar-type scalar-vtn-h this-vtn-h)
                  (mopr:value-type-name-get-as-token scalar-token-h scalar-vtn-h))
                (mopr:value-type-name-get-as-token scalar-token-h this-vtn-h))
            (setf (getf info-args :type-key)
                  (alexandria:format-symbol
                   "KEYWORD" "~A" (mopr:token-cstr scalar-token-h))))))
    (apply #'make-instance info-type
           :base-name prop-name
           :variability (mopr:property-definition-get-variability prop-def-h)
           info-args)))

(defun create-property-info-table (prim-token-h schema-type
                                   &aux
                                     prim-def-fn
                                     (table (make-hash-table)))
  (setf prim-def-fn
        (case schema-type
          (:api #'mopr:prim-definition-ctor-api)
          (:isa #'mopr:prim-definition-ctor-isa)
          (otherwise (error "Unknown keyword for schema type!"))))

  (mopr:with-handles* ((prim-def-h :prim-definition)
                       (prop-token-h :token)
                       (prop-def-h :property-definition))
    (funcall prim-def-fn prim-def-h prim-token-h)
    (when (zerop (mopr:prim-definition-is-empty-p prim-def-h))
      (loop for i below (mopr:prim-definition-get-property-count prim-def-h)
            do (progn
                 (mopr:prim-definition-get-property-name prop-token-h prim-def-h i)
                 (mopr:prim-definition-get-property prop-def-h prim-def-h prop-token-h)
                 (let* ((prop-name (mopr:token-cstr prop-token-h))
                        (prop-name-kw (alexandria:format-symbol "KEYWORD" "~A" prop-name))
                        (prop-name-kw-up (alexandria:format-symbol "KEYWORD" "~:@(~A~)" prop-name))
                        (info (generate-prop-info prop-name prop-def-h)))
                   (setf (gethash prop-name-kw table) info
                         (gethash prop-name-kw-up table) info))))))
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
                    (let ((prim-token-h (mopr:create-token)))
                      (mopr:token-ctor-cstr
                       prim-token-h
                       schema-name)
                      prim-token-h))
                   (prop-table (create-property-info-table
                                name-token type)))))
  "..."
  (name-token
   (error "SCHEMA should have a NAME-TOKEN.")
   :type mopr:mopr-token-h)
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

(defun create-generic-schemas (schema-type bundle)
  (mopr:with-handles* ((type-set-h :schema-type-set)
                       (schema-info-h :schema-info)
                       (family-token-h :token)
                       (id-token-h :token))
    (funcall
     (case schema-type
       (:api #'mopr:schema-type-set-ctor-api-derived)
       (:isa #'mopr:schema-type-set-ctor-isa-derived)
       (otherwise (error "Unknown keyword for schema type!")))
     type-set-h)
    (loop for i below (mopr:schema-type-set-get-type-count type-set-h)
          do (mopr:schema-type-set-get-schema-info schema-info-h type-set-h i)
          when (and (zerop (mopr:schema-info-is-empty-p schema-info-h))
                    (not (member (mopr:schema-info-get-kind schema-info-h)
                                 +ignored-schema-kinds+)))
            do (progn
                 (mopr:schema-info-get-family family-token-h schema-info-h)
                 (mopr:schema-info-get-identifier id-token-h schema-info-h)
                 (let ((id (mopr:token-cstr id-token-h)))
                   (mopr-reg:add-entry
                    bundle
                    (alexandria:format-symbol "KEYWORD" "~A" id)
                    (make-schema id
                                 (alexandria:format-symbol
                                  "KEYWORD" "~A"
                                  (mopr:token-cstr family-token-h))
                                 schema-type
                                 (mopr:schema-info-get-kind schema-info-h)
                                 (mopr:schema-info-get-version schema-info-h)))))
          end)))

(defmethod mopr-reg:teardown-entry ((val schema)
                                  &aux (tok (schema-name-token val)))
  ;; (format t "DELETING SCHEMA: ~A~%" val)
  (mopr:delete-token tok)
  (autowrap:invalidate tok))
