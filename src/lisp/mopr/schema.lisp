;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-scm)

(defconstant +isa-schema-list+
  (list 'mopr-ns:|PointInstancer|
        'mopr-ns:|Camera|
        'mopr-ns:|Xform|
        'mopr-ns:|Mesh|
        'mopr-ns:|NurbsPatch|
        'mopr-ns:|BasisCurves|
        'mopr-ns:|NurbsCurves|
        'mopr-ns:|Points|
        'mopr-ns:|Capsule|
        'mopr-ns:|Cone|
        'mopr-ns:|Cube|
        'mopr-ns:|Cylinder|
        'mopr-ns:|Sphere|))

(defconstant +api-schema-list+
  (list 'mopr-ns:|CollectionAPI|
        'mopr-ns:|ClipsAPI|))

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
                        (info (generate-prop-info prop-name prop-def-h)))
                   (setf (gethash prop-name-kw table) info))))))
  table)

(defstruct (schema (:type vector) :named
                   (:constructor make-schema
                       (schema-name
                        schema-type
                        &aux
                          (name-token
                           (let ((prim-token-h (mopr:create-token)))
                             (mopr:token-ctor-cstr
                              prim-token-h
                              schema-name)
                             prim-token-h))
                          (prop-table (create-property-info-table
                                       name-token schema-type)))))
  "..."
  (name-token
   (error "SCHEMA should have a NAME-TOKEN.")
   :type mopr:mopr-token-h)
  (schema-type
   (error "SCHEMA should have a SCHEMA-TYPE.")
   :type (member :isa :api))
  (prop-table
   (error "SCHEMA should have a PROP-TABLE.")
   :type hash-table))

(defun create-generic-isa-schema-table (table)
  (loop for s in +isa-schema-list+
        for s-upcase = (alexandria:format-symbol "MOPR-NS" "~:@(~A~)" s)
        for s-name = (symbol-name s)
        for val = (make-schema s-name :isa)
        do (setf (gethash s table) val)
        do (setf (gethash s-upcase table) val)))

(defun delete-generic-isa-schema-table (table)
  (loop for s in +isa-schema-list+
        for val = (schema-name-token (gethash s table))
        do (progn
             (mopr:delete-token val)
             (autowrap:invalidate val))))

(defun create-generic-api-schema-table (table)
  (loop for s in +api-schema-list+
        for s-upcase = (alexandria:format-symbol "MOPR-NS" "~:@(~A~)" s)
        for s-name = (symbol-name s)
        for val = (make-schema s-name :api)
        do (setf (gethash s table) val)
        do (setf (gethash s-upcase table) val)))

(defun delete-generic-api-schema-table (table)
  (loop for s in +api-schema-list+
        for val = (schema-name-token (gethash s table))
        do (progn
             (mopr:delete-token val)
             (autowrap:invalidate val))))
