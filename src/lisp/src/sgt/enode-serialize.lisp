;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

;;
;;; Mapping between ENODE and USDS Forms
;;

(defgeneric extract-payload (payload-class form)
  (:documentation "Generate the payload that corresponds to USDS form."))

(defgeneric list-enode-children (payload-class form)
  (:documentation "Get the form that corresponds to the serialized children for given payload class."))

;; DEBUG :

;; (defmethod extract-payload :around (payload-class form)
;;   (format t "~%EXTRACT-PAYLOAD: ~S ~S ~%" payload-class form)
;;   (call-next-method))

(defgeneric payload-serialize (payload)
  (:documentation "Get the list that represents the enode in USDS form."))

(defun enode-serialize (node)
  (nconc
   (payload-serialize (enode-payload node))
   (loop for ch across (enode-children node)
         collecting (enode-serialize ch))))

(defmethod payload-serialize ((payload root-container))
  nil)

(defmethod extract-payload ((payload-class (eql 'root-container)) form)
  (declare (ignore form))
  (make-root-container))

(defmethod list-enode-children ((payload-class (eql 'root-container)) form)
  form)

(defmethod payload-serialize ((payload group-container))
  `(:group))

(defmethod extract-payload ((payload-class (eql 'group-container)) form)
  (declare (ignore form))
  (make-group-container))

(defmethod list-enode-children ((payload-class (eql 'group-container)) form)
  form)

(defmethod payload-serialize ((payload var-directive))
  `(:var
    ,(var-directive-name-param payload)
    ,(var-directive-aux-form-param payload)
    ,@(var-directive-val-form-param payload)))

(defmethod extract-payload ((payload-class (eql 'var-directive)) form)
  (make-var-directive
   :name-param (car form)
   :aux-form-param (cadr form)
   :val-form-param (cddr form)))

(defmethod payload-serialize ((payload each-directive))
  `(:each
    ,(each-directive-name-param payload)
    ,(each-directive-keys-form-param payload)
    ,@(each-directive-vals-form-param payload)))

(defmethod extract-payload ((payload-class (eql 'each-directive)) form)
  (make-each-directive
   :name-param (car form)
   :keys-form-param (cadr form)
   :vals-form-param (cddr form)))

(defmethod payload-serialize ((payload iota-directive))
  `(:iota
    ,(iota-directive-name-param payload)
    ,(iota-directive-key-param payload)
    ,(iota-directive-end-param payload)
    ,@(if (iota-directive-start-param payload) (list (iota-directive-start-param payload)))
    ,@(if (iota-directive-step-param payload) (list (iota-directive-step-param payload)))))

(defmethod extract-payload ((payload-class (eql 'iota-directive)) form &aux (len (length form)))
  (make-iota-directive
   :name-param (nth 0 form)
   :key-param (nth 1 form)
   :end-param (nth 2 form)
   :start-param (if (> len 3) (nth 3 form))
   :step-param (if (> len 4) (nth 4 form))))

(defmethod payload-serialize ((payload call-directive))
  `(:call
    ,(call-directive-aux-form-param payload)
    ,@(call-directive-body-form-param payload)))

(defmethod extract-payload ((payload-class (eql 'call-directive)) form)
  (make-call-directive
   :aux-form-param (car form)
   :body-form-param (cdr form)))

(defmethod extract-payload ((payload-class (eql 'prim-call-directive)) form)
  (make-prim-call-directive
   :aux-form-param (car form)
   :body-form-param (cdr form)))

(defmethod payload-serialize ((payload prim-type-statement))
  `(:type
    ,(prim-type-statement-name-param payload)))

(defmethod extract-payload ((payload-class (eql 'prim-type-statement)) form)
  (make-prim-type-statement
   :name-param (car form)))

(defmethod payload-serialize ((payload prim-attr-statement))
  `(:attr
    ,(if (prim-attr-statement-meta-form-param payload)
         (cons
          (prim-attr-statement-name-param payload)
          (prim-attr-statement-meta-form-param payload))
         (prim-attr-statement-name-param payload))
    ,(prim-attr-statement-category-param payload)
    ,(prim-attr-statement-type-param payload)
    ,@(prim-attr-statement-body-form-param payload)))

(defun map-property-name-and-meta (data)
  (etypecase data
    (symbol (list :name-param data :meta-form-param nil))
    (string (list :name-param (coerce data 'base-string) :meta-form-param nil))
    (list (list :name-param (etypecase (car data)
                              (symbol (car data))
                              (string (coerce (car data) 'base-string)))
                :meta-form-param (cdr data)))))

(defmethod extract-payload ((payload-class (eql 'prim-attr-statement)) form &aux (data (car form)))
  (apply
   'make-prim-attr-statement
   :category-param (cadr form)
   :type-param (caddr form)
   :body-form-param (cdddr form)
   (map-property-name-and-meta data)))

(defmethod payload-serialize ((payload prim-rel-statement))
  `(:rel
    ,(if (prim-rel-statement-meta-form-param payload)
         (cons
          (prim-rel-statement-name-param payload)
          (prim-rel-statement-meta-form-param payload))
         (prim-rel-statement-name-param payload))
    ,@(prim-rel-statement-body-form-param payload)))

(defmethod extract-payload ((payload-class (eql 'prim-rel-statement)) form &aux (data (car form)))
  (apply
   'make-prim-rel-statement
   :body-form-param (cdr form)
   (map-property-name-and-meta data)))

(defmethod payload-serialize ((payload prim-ns-container))
  `(:ns
    ,(prim-ns-container-name-param payload)))

(defmethod extract-payload ((payload-class (eql 'prim-ns-container)) form)
  (make-prim-ns-container
   :name-param (coerce (car form) 'base-string)))

(defmethod list-enode-children ((payload-class (eql 'prim-ns-container)) form)
  (cdr form))

(defmethod payload-serialize ((payload prim-statement))
  `(:prim
    ,(prim-statement-path-form-param payload)))

(defmethod extract-payload ((payload-class (eql 'prim-statement)) form)
  (make-prim-statement
   :path-form-param (car form)))

(defmethod list-enode-children ((payload-class (eql 'prim-statement)) form)
  (cdr form))

(defmethod payload-serialize ((payload tree-statement))
  `(:tree
    ,@(tree-statement-body-form-param payload)))

(defmethod extract-payload ((payload-class (eql 'tree-statement)) form)
  (make-tree-statement
   :body-form-param form))

(defmethod payload-serialize ((payload meta-statement))
  `(:meta
    ,@(meta-statement-body-form-param payload)))

(defmethod extract-payload ((payload-class (eql 'meta-statement)) form)
  (make-meta-statement
   :body-form-param form))

(defmethod extract-payload ((payload-class (eql 'prim-meta-statement)) form)
  (make-prim-meta-statement
   :body-form-param form))

;;
;;; Form Handlers
;;

(defvar *debug-mode* t)

(defun unknown-form-error (form action)
  (format t "
[ERROR] Cannot handle form.
[  -  ] FORM: ~A
[  -  ] ACTION: ~A
"
          form
          (case action
            (:skip "Skipping.")
            (:debug  (if *debug-mode*
                         "Debug mode enabled: Will error."
                         "Debug mode disabled: Skipping."))
            (:error  "Will error.")
            (otherwise (error "Coding error. Unknown message action."))))
  (when (and (eq action :debug) *debug-mode*)
    (error "Cannot handle form: ~S~%" form)))

(defun generate-enode (parent component-classes payload)
  (make-instance
   'enode
   :parent parent
   :payload payload
   :components (loop for cc in component-classes collecting (make-instance cc))))

(defun generate-wired-enode (parent component-classes payload
                             &aux (n (generate-enode parent component-classes payload)))
  (vector-push-extend n (enode-children parent))
  n)

(defun deserialize-var-form (parent form component-classes)
  ;; (format t "~%Called deserialize-var-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'var-directive form)))

(defun deserialize-each-form (parent form component-classes)
  ;; (format t "~%Called deserialize-each-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'each-directive form)))

(defun deserialize-iota-form (parent form component-classes)
  ;; (format t "~%Called deserialize-iota-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'iota-directive form)))

(defun deserialize-call-form (parent form component-classes)
  ;; (format t "~%Called deserialize-call-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'call-directive form)))

(defun deserialize-prim-call-form (parent form component-classes)
  ;; (format t "~%Called deserialize-call-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'prim-call-directive form)))

(defun deserialize-prim-type-form (parent form component-classes)
  ;; (format t "~%Called deserialize-type-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'prim-type-statement form)))

(defun deserialize-prim-meta-form (parent form component-classes)
  ;; (format t "~%Called deserialize-prim-meta-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'prim-meta-statement form)))

(defun deserialize-prim-attr-form (parent form component-classes)
  ;; (format t "~%Called deserialize-prim-attr-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'prim-attr-statement form)))

(defun deserialize-prim-rel-form (parent form component-classes)
  ;; (format t "~%Called deserialize-prim-rel-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'prim-rel-statement form)))

(defun deserialize-prim-ns-form (parent form component-classes)
  ;; (format t "~%Called deserialize-prim-ns-form!~%: ~S~%" form)
  (let* ((nn (generate-wired-enode parent component-classes
                                   (extract-payload 'prim-ns-container form))))
    (loop for l in (list-enode-children 'prim-ns-container form)
          for fn = (case (car l)
                     (:attr   #'deserialize-prim-attr-form)
                     (:|attr| #'deserialize-prim-attr-form)
                     (:rel    #'deserialize-prim-rel-form)
                     (:|rel|  #'deserialize-prim-rel-form)
                     (:ns     #'deserialize-prim-ns-form)
                     (:|ns|   #'deserialize-prim-ns-form))
          do (if fn
                 (funcall fn nn (cdr l) component-classes)
                 (unknown-form-error (car l) :debug)))))

(defun deserialize-prim-subforms (pn subforms component-classes)
  ;; (format t "~%Called deserialize-prim-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for fn = (case (car l)
                   ;; TODO : Handle other forms.
                   (:group   #'deserialize-prim-group-form)
                   (:|group| #'deserialize-prim-group-form)
                   (:call    #'deserialize-prim-call-form)
                   (:|call|  #'deserialize-prim-call-form)
                   (:type    #'deserialize-prim-type-form)
                   (:|type|  #'deserialize-prim-type-form)
                   (:meta    #'deserialize-prim-meta-form)
                   (:|meta|  #'deserialize-prim-meta-form)
                   (:attr    #'deserialize-prim-attr-form)
                   (:|attr|  #'deserialize-prim-attr-form)
                   (:rel     #'deserialize-prim-rel-form)
                   (:|rel|   #'deserialize-prim-rel-form)
                   (:ns      #'deserialize-prim-ns-form)
                   (:|ns|    #'deserialize-prim-ns-form))
        do (if fn
               (funcall fn pn (cdr l) component-classes)
               (unknown-form-error (car l) :debug))))

(defun deserialize-prim-form (parent form component-classes)
  ;; (format t "~%Called deserialize-prim-form!~%: ~S~%" form)
  (let* ((pn (generate-wired-enode parent component-classes
                                   (extract-payload 'prim-statement form))))
    (deserialize-prim-subforms pn (list-enode-children 'prim-statement form) component-classes)))

(defun deserialize-group-form-generic (parent form component-classes fn)
  (let* ((gn (generate-wired-enode parent component-classes
                                   (extract-payload 'group-container form))))
    (funcall fn gn (list-enode-children 'group-container form) component-classes)))

(defun deserialize-prim-group-form (parent form component-classes)
  ;; (format t "~%Called deserialize-prim-group-form!~%: ~S~%" form)
  (deserialize-group-form-generic parent form component-classes #'deserialize-prim-subforms))

(defun deserialize-tree-form (parent form component-classes)
  ;; (format t "~%Called deserialize-tree-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'tree-statement form)))

(defun deserialize-meta-form (parent form component-classes)
  ;; (format t "~%Called deserialize-meta-form!~%: ~S~%" form)
  (generate-wired-enode parent component-classes
                        (extract-payload 'meta-statement form)))

(defun deserialize-group-form (parent form component-classes)
  ;; (format t "~%Called deserialize-group-form!~%: ~S~%" form)
  (deserialize-group-form-generic parent form component-classes #'deserialize-data-subforms))

(defun deserialize-data-subforms (parent subforms component-classes)
  ;; (format t "~%Called deserialize-data-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for fn = (case (car l)
                   (:group   #'deserialize-group-form)
                   (:|group| #'deserialize-group-form)
                   (:var     #'deserialize-var-form)
                   (:|var|   #'deserialize-var-form)
                   (:each    #'deserialize-each-form)
                   (:|each|  #'deserialize-each-form)
                   (:iota    #'deserialize-iota-form)
                   (:|iota|  #'deserialize-iota-form)
                   (:call    #'deserialize-call-form)
                   (:|call|  #'deserialize-call-form)
                   (:meta    #'deserialize-meta-form)
                   (:|meta|  #'deserialize-meta-form)
                   (:tree    #'deserialize-tree-form)
                   (:|tree|  #'deserialize-tree-form)
                   (:prim    #'deserialize-prim-form)
                   (:|prim|  #'deserialize-prim-form))
        do (if fn
               (funcall fn parent (cdr l) component-classes)
               (unknown-form-error (car l) :debug))))

;;
;;; Top-Level API and Macros
;;

(defun deserialize (usds-data component-classes
                    &aux
                      (rn (generate-enode nil component-classes
                                          (extract-payload 'root-container usds-data))))
  (deserialize-data-subforms rn (list-enode-children 'root-container usds-data) component-classes)

  ;; DEBUG :

  ;; (format t "~%COMPARISON: ~S ~%"
  ;;         (string-equal
  ;;          (format nil "~S" usds-data)
  ;;          (format nil "~S" (enode-serialize rn))))

  rn)

(defun read-from-usds-file (filepath read-pkg &optional component-classes)
  "CAUTION: Even though READ-EVAL is disabled, relying on READ for data is still dangerous!"
  (with-open-file (in filepath)
    (with-standard-io-syntax
      (let ((*package* read-pkg)
            ;; Assignments based on uiop/stream:with-safe-io-syntax .
            (*print-readably* nil)
            (*read-eval* nil))
        (deserialize (read in nil) component-classes)))))
