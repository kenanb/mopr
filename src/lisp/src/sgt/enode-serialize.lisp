;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

;;
;;; Mapping between ENODE and USDS Forms
;;

(defgeneric enode-serialize (node)
  (:documentation "Get the list that represents the enode in USDS form."))

(defmethod enode-serialize ((n enode))
  (loop for c across (enode-children n)
        collecting (enode-serialize c)))

(defmethod enode-serialize ((n root-enode))
  (call-next-method))

(defun root-form-params (form)
  (declare (ignore form))
  nil)

(defun root-form-children (form)
  form)

(defmethod enode-serialize ((n group-enode))
  `(:group
    ,@(call-next-method)))

(defun group-form-params (form)
  (declare (ignore form))
  nil)

(defun group-form-children (form)
  form)

(defmethod enode-serialize ((n var-enode))
  `(:var
    ,(var-enode-name-param n)
    ,(var-enode-aux-form-param n)
    ,@(var-enode-val-form-param n)
    ,@(call-next-method)))

(defun var-form-params (form)
  (list :name-param (car form)
        :aux-form-param (cadr form)
        :val-form-param (cddr form)))

(defmethod enode-serialize ((n each-enode))
  `(:each
    ,(each-enode-name-param n)
    ,(each-enode-keys-form-param n)
    ,@(each-enode-vals-form-param n)
    ,@(call-next-method)))

(defun each-form-params (form)
  (list :name-param (car form)
        :keys-form-param (cadr form)
        :vals-form-param (cddr form)))

(defmethod enode-serialize ((n iota-enode))
  `(:iota
    ,(iota-enode-name-param n)
    ,(iota-enode-key-param n)
    ,(iota-enode-end-param n)
    ,@(if (iota-enode-start-param n) (list (iota-enode-start-param n)))
    ,@(if (iota-enode-step-param n) (list (iota-enode-step-param n)))
    ,@(call-next-method)))

(defun iota-form-params (form &aux (len (length form)))
  (list :name-param (nth 0 form)
        :key-param (nth 1 form)
        :end-param (nth 2 form)
        :start-param (if (> len 3) (nth 3 form))
        :step-param (if (> len 4) (nth 4 form))))

(defmethod enode-serialize ((n call-enode))
  `(:call
    ,(call-enode-aux-form-param n)
    ,@(call-enode-body-form-param n)
    ,@(call-next-method)))

(defun call-form-params (form)
  (list :aux-form-param (car form)
        :body-form-param (cdr form)))

(defmethod enode-serialize ((n prim-type-enode))
  `(:type
    ,(prim-type-enode-name-param n)
    ,@(call-next-method)))

(defun prim-type-form-params (form)
  (list :name-param (car form)))

(defmethod enode-serialize ((n prim-attr-enode))
  `(:attr
    ,(if (prim-attr-enode-meta-form-param n)
         (cons
          (prim-attr-enode-name-param n)
          (prim-attr-enode-meta-form-param n))
         (prim-attr-enode-name-param n))
    ,(prim-attr-enode-category-param n)
    ,(prim-attr-enode-type-param n)
    ,@(prim-attr-enode-body-form-param n)
    ,@(call-next-method)))

(defun prim-attr-form-params (form &aux (data (car form)))
  (nconc
   (etypecase data
     (symbol (list :name-param data :meta-form-param nil))
     (string (list :name-param data :meta-form-param nil))
     (list (list :name-param (car data) :meta-form-param (cdr data))))
   (list :category-param (cadr form)
         :type-param (caddr form)
         :body-form-param (cdddr form))))

(defmethod enode-serialize ((n prim-rel-enode))
  `(:rel
    ,(if (prim-rel-enode-meta-form-param n)
         (cons
          (prim-rel-enode-name-param n)
          (prim-rel-enode-meta-form-param n))
         (prim-rel-enode-name-param n))
    ,@(prim-rel-enode-body-form-param n)
    ,@(call-next-method)))

(defun prim-rel-form-params (form &aux (data (car form)))
  (nconc
   (etypecase data
     (symbol (list :name-param data :meta-form-param nil))
     (string (list :name-param data :meta-form-param nil))
     (list (list :name-param (car data) :meta-form-param (cdr data))))
   (list :body-form-param (cdr form))))

(defmethod enode-serialize ((n prim-ns-enode))
  `(:ns
    ,(prim-ns-enode-name-param n)
    ,@(call-next-method)))

(defun prim-ns-form-params (form)
  (list :name-param (car form)))

(defun prim-ns-form-children (form)
  (cdr form))

(defmethod enode-serialize ((n prim-enode))
  `(:prim
    ,(prim-enode-path-form-param n)
    ,@(call-next-method)))

(defun prim-form-params (form)
  (list :path-form-param (car form)))

(defun prim-form-children (form)
  (cdr form))

(defmethod enode-serialize ((n tree-enode))
  `(:tree
    ,@(tree-enode-body-form-param n)
    ,@(call-next-method)))

(defun tree-form-params (form)
  (list :body-form-param form))

(defmethod enode-serialize ((n meta-enode))
  `(:meta
    ,@(meta-enode-body-form-param n)
    ,@(call-next-method)))

(defun meta-form-params (form)
  (list :body-form-param form))

;;
;;; Unified Deserialization APIs
;;

(defun make-enode-instance (class form &key parent ext-classes)
  (apply #'make-instance class
         :parent parent
         :extensions (loop for e in ext-classes collecting (make-instance e))
         (funcall
          (case class
            ('root-enode #'root-form-params)
            ('group-enode #'group-form-params)
            ('var-enode #'var-form-params)
            ('each-enode #'each-form-params)
            ('iota-enode #'iota-form-params)
            ('call-enode #'call-form-params)
            ('prim-call-enode #'call-form-params)
            ('prim-type-enode #'prim-type-form-params)
            ('prim-attr-enode #'prim-attr-form-params)
            ('prim-rel-enode #'prim-rel-form-params)
            ('prim-meta-enode #'meta-form-params)
            ('prim-ns-enode #'prim-ns-form-params)
            ('prim-enode #'prim-form-params)
            ('tree-enode #'tree-form-params)
            ('meta-enode #'meta-form-params))
          form)))

(defun list-enode-children (class form)
  (funcall
   (case class
     ('root-enode #'root-form-children)
     ('group-enode #'group-form-children)
     ('prim-enode #'prim-form-children)
     ('prim-ns-enode #'prim-ns-form-children))
   form))

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

(defun deserialize-var-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-var-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'var-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-each-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-each-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'each-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-iota-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-iota-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'iota-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-call-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-call-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'call-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-prim-call-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-call-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-call-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-prim-type-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-type-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-type-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-prim-meta-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-prim-meta-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-meta-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-prim-attr-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-prim-attr-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-attr-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-prim-rel-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-prim-rel-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-rel-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-prim-ns-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-prim-ns-form!~%: ~S~%" form)
  (let* ((nn (make-enode-instance 'prim-ns-enode form
                                  :parent parent
                                  :ext-classes ext-classes)))
    (vector-push-extend nn (enode-children parent))
    (loop for l in (list-enode-children 'prim-ns-enode form)
          for fn = (case (car l)
                     (:attr   #'deserialize-prim-attr-form)
                     (:|attr| #'deserialize-prim-attr-form)
                     (:rel    #'deserialize-prim-rel-form)
                     (:|rel|  #'deserialize-prim-rel-form)
                     (:ns     #'deserialize-prim-ns-form)
                     (:|ns|   #'deserialize-prim-ns-form))
          do (if fn
                 (funcall fn nn (cdr l) ext-classes)
                 (unknown-form-error (car l) :debug)))))

(defun deserialize-prim-subforms (pn subforms ext-classes)
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
               (funcall fn pn (cdr l) ext-classes)
               (unknown-form-error (car l) :debug))))

(defun deserialize-prim-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-prim-form!~%: ~S~%" form)
  (let* ((pn (make-enode-instance 'prim-enode form
                                  :parent parent
                                  :ext-classes ext-classes)))
    (vector-push-extend pn (enode-children parent))
    (deserialize-prim-subforms pn (list-enode-children 'prim-enode form) ext-classes)))

(defun deserialize-group-form-generic (parent form ext-classes fn)
  (let* ((gn (make-enode-instance 'group-enode form
                                  :parent parent
                                  :ext-classes ext-classes)))
    (vector-push-extend gn (enode-children parent))
    (funcall fn gn (list-enode-children 'group-enode form) ext-classes)))

(defun deserialize-prim-group-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-prim-group-form!~%: ~S~%" form)
  (deserialize-group-form-generic parent form ext-classes #'deserialize-prim-subforms))

(defun deserialize-tree-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-tree-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'tree-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-meta-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-meta-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'meta-enode form
                                           :parent parent
                                           :ext-classes ext-classes)
                      (enode-children parent)))

(defun deserialize-group-form (parent form ext-classes)
  ;; (format t "~%Called deserialize-group-form!~%: ~S~%" form)
  (deserialize-group-form-generic parent form ext-classes #'deserialize-data-subforms))

(defun deserialize-data-subforms (parent subforms ext-classes)
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
               (funcall fn parent (cdr l) ext-classes)
               (unknown-form-error (car l) :debug))))

;;
;;; Top-Level API and Macros
;;

(defun deserialize (usds-data ext-classes
                    &aux
                      (rn (make-enode-instance 'root-enode usds-data
                                               :ext-classes ext-classes)))
  (deserialize-data-subforms rn (list-enode-children 'root-enode usds-data) ext-classes)
  rn)
