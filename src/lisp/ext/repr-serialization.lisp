;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr-serialization
  (:import-from :mopr)
  (:import-from :mopr-ext/repr-shared
                #:with-layout-settings)
  (:import-from :mopr-ext/repr-rdata)
  (:use :mopr-ext/repr-rnode)
  (:use :cl)
  (:export
   #:deserialize-call-enabled
   #:enode-serialize))

(in-package :mopr-ext/repr-serialization)

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
    ,@(call-next-method)))

(defun iota-form-params (form)
  (list :name-param (car form)
        :key-param (cadr form)
        :end-param (caddr form)))

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

(defun make-enode-instance (class rparent form)
  (apply #'make-instance class
         :parent rparent
         (funcall
          (case class
            ('root-enode #'root-form-params)
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
     ('prim-enode #'prim-form-children)
     ('prim-ns-enode #'prim-ns-form-children))
   form))

;;
;;; Form Handlers
;;

(defvar *debug-mode* t)

(defvar *enable-call* nil)

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

(defun handle-var-form (rparent form)
  ;; (format t "~%Called handle-var-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'var-enode rparent form)
                      (enode-children rparent)))

(defun handle-each-form (rparent form)
  ;; (format t "~%Called handle-each-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'each-enode rparent form)
                      (enode-children rparent)))

(defun handle-iota-form (rparent form)
  ;; (format t "~%Called handle-iota-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'iota-enode rparent form)
                      (enode-children rparent)))

(defun handle-call-form (rparent form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'call-enode rparent form)
                      (enode-children rparent)))

(defun handle-prim-call-form (rparent form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-call-enode rparent form)
                      (enode-children rparent)))

(defun handle-prim-type-form (rparent form)
  ;; (format t "~%Called handle-type-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-type-enode rparent form)
                      (enode-children rparent)))

(defun handle-prim-meta-form (rparent form)
  ;; (format t "~%Called handle-prim-meta-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-meta-enode rparent form)
                      (enode-children rparent)))

(defun handle-prim-attr-form (rparent form)
  ;; (format t "~%Called handle-prim-attr-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-attr-enode rparent form)
                      (enode-children rparent)))

(defun handle-prim-rel-form (rparent form)
  ;; (format t "~%Called handle-prim-rel-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'prim-rel-enode rparent form)
                      (enode-children rparent)))

(defun handle-prim-ns-form (rparent form)
  ;; (format t "~%Called handle-prim-ns-form!~%: ~S~%" form)
  (let* ((nn (make-enode-instance 'prim-ns-enode rparent form)))
    (vector-push-extend nn (enode-children rparent))
    (loop for l in (list-enode-children 'prim-ns-enode form)
          for fn = (case (car l)
                     (:attr   #'handle-prim-attr-form)
                     (:|attr| #'handle-prim-attr-form)
                     (:rel    #'handle-prim-rel-form)
                     (:|rel|  #'handle-prim-rel-form)
                     (:ns     #'handle-prim-ns-form)
                     (:|ns|   #'handle-prim-ns-form))
          do (if fn
                 (funcall fn nn (cdr l))
                 (unknown-form-error (car l) :debug)))))

(defun handle-prim-subforms (pn subforms)
  ;; (format t "~%Called handle-prim-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for fn = (case (car l)
                   ;; TODO : Handle other forms.
                   (:call   #'handle-prim-call-form)
                   (:|call| #'handle-prim-call-form)
                   (:type   #'handle-prim-type-form)
                   (:|type| #'handle-prim-type-form)
                   (:meta   #'handle-prim-meta-form)
                   (:|meta| #'handle-prim-meta-form)
                   (:attr   #'handle-prim-attr-form)
                   (:|attr| #'handle-prim-attr-form)
                   (:rel    #'handle-prim-rel-form)
                   (:|rel|  #'handle-prim-rel-form)
                   (:ns     #'handle-prim-ns-form)
                   (:|ns|   #'handle-prim-ns-form))
        do (if fn
               (funcall fn pn (cdr l))
               (unknown-form-error (car l) :debug))))

(defun handle-prim-form (rparent form)
  ;; (format t "~%Called handle-prim-form!~%: ~S~%" form)
  (let* ((pn (make-enode-instance 'prim-enode rparent form)))
    (vector-push-extend pn (enode-children rparent))
    (handle-prim-subforms pn (list-enode-children 'prim-enode form))))

(defun handle-tree-form (rparent form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'tree-enode rparent form)
                      (enode-children rparent)))

(defun handle-meta-form (rparent form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)
  (vector-push-extend (make-enode-instance 'meta-enode rparent form)
                      (enode-children rparent)))

(defun handle-data-subforms (rparent subforms)
  ;; (format t "~%Called handle-data-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for fn = (case (car l)
                   (:var    #'handle-var-form)
                   (:|var|  #'handle-var-form)
                   (:each   #'handle-each-form)
                   (:|each| #'handle-each-form)
                   (:iota   #'handle-iota-form)
                   (:|iota| #'handle-iota-form)
                   (:call   #'handle-call-form)
                   (:|call| #'handle-call-form)
                   (:meta   #'handle-meta-form)
                   (:|meta| #'handle-meta-form)
                   (:tree   #'handle-tree-form)
                   (:|tree| #'handle-tree-form)
                   (:prim   #'handle-prim-form)
                   (:|prim| #'handle-prim-form))
        do (if fn
               (funcall fn rparent (cdr l))
               (unknown-form-error (car l) :debug))))

;;
;;; Top-Level API and Macros
;;

(defmacro with-repr-variables ((&key
                                  (enable-call nil))
                               &body body)
  `(let* ((*enable-call* ,enable-call))
     ,@body))

(defun deserialize-call-enabled (usds-data)
  (with-layout-settings
      (mopr-info:with-registry (:supported-cases '(:upcase))
        (mopr-plug:with-configuration ()
          (with-repr-variables (:enable-call t)
            (let* ((rn (make-enode-instance 'root-enode nil usds-data)))
              (handle-data-subforms rn (list-enode-children 'root-enode usds-data))
              rn))))))
