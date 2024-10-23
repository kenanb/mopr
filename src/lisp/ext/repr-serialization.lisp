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
   #:rnode-serialize))

(in-package :mopr-ext/repr-serialization)

;;
;;; Mapping between RNODE and USDS Forms
;;

(defgeneric rnode-serialize (node)
  (:documentation "Get the list that represents the rnode in USDS form."))

(defmethod rnode-serialize ((n rnode))
  (loop for c across (rnode-children n)
        collecting (rnode-serialize c)))

(defmethod rnode-serialize ((n root-rnode))
  (call-next-method))

(defun root-form-params (form)
  (declare (ignore form))
  nil)

(defun root-form-children (form)
  form)

(defmethod rnode-serialize ((n var-rnode))
  `(:var
    ,(var-rnode-name-param n)
    ,(var-rnode-aux-form-param n)
    ,@(var-rnode-val-form-param n)
    ,@(call-next-method)))

(defun var-form-params (form)
  (list :name-param (car form)
        :aux-form-param (cadr form)
        :val-form-param (cddr form)))

(defmethod rnode-serialize ((n each-rnode))
  `(:each
    ,(each-rnode-name-param n)
    ,(each-rnode-keys-form-param n)
    ,@(each-rnode-vals-form-param n)
    ,@(call-next-method)))

(defun each-form-params (form)
  (list :name-param (car form)
        :keys-form-param (cadr form)
        :vals-form-param (cddr form)))

(defmethod rnode-serialize ((n iota-rnode))
  `(:iota
    ,(iota-rnode-name-param n)
    ,(iota-rnode-key-param n)
    ,(iota-rnode-end-param n)
    ,@(call-next-method)))

(defun iota-form-params (form)
  (list :name-param (car form)
        :key-param (cadr form)
        :end-param (caddr form)))

(defmethod rnode-serialize ((n call-rnode))
  `(:call
    ,(call-rnode-aux-form-param n)
    ,@(call-rnode-body-form-param n)
    ,@(call-next-method)))

(defun call-form-params (form)
  (list :aux-form-param (car form)
        :body-form-param (cdr form)))

(defmethod rnode-serialize ((n prim-type-rnode))
  `(:type
    ,(prim-type-rnode-name-param n)
    ,@(call-next-method)))

(defun prim-type-form-params (form)
  (list :name-param (car form)))

(defmethod rnode-serialize ((n prim-rnode))
  `(:prim
    ,(prim-rnode-path-form-param n)
    ,@(call-next-method)))

(defun prim-form-params (form)
  (list :path-form-param (car form)))

(defun prim-form-children (form)
  (cdr form))

(defmethod rnode-serialize ((n tree-rnode))
  `(:tree
    ,@(tree-rnode-body-form-param n)
    ,@(call-next-method)))

(defun tree-form-params (form)
  (list :body-form-param form))

(defmethod rnode-serialize ((n meta-rnode))
  `(:meta
    ,@(meta-rnode-body-form-param n)
    ,@(call-next-method)))

(defun meta-form-params (form)
  (list :body-form-param form))

;;
;;; Unified Deserialization APIs
;;

(defun make-rnode-instance (class rparent form)
  (apply #'make-instance class
         :parent rparent
         (funcall
          (case class
            ('root-rnode #'root-form-params)
            ('var-rnode #'var-form-params)
            ('each-rnode #'each-form-params)
            ('iota-rnode #'iota-form-params)
            ('call-rnode #'call-form-params)
            ('prim-call-rnode #'call-form-params)
            ('prim-type-rnode #'prim-type-form-params)
            ('prim-rnode #'prim-form-params)
            ('tree-rnode #'tree-form-params)
            ('meta-rnode #'meta-form-params))
          form)))

(defun list-rnode-children (class form)
  (funcall
   (case class
     ('root-rnode #'root-form-children)
     ('prim-rnode #'prim-form-children))
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
  (vector-push-extend (make-rnode-instance 'var-rnode rparent form)
                      (rnode-children rparent)))

(defun handle-each-form (rparent form)
  ;; (format t "~%Called handle-each-form!~%: ~S~%" form)
  (vector-push-extend (make-rnode-instance 'each-rnode rparent form)
                      (rnode-children rparent)))

(defun handle-iota-form (rparent form)
  ;; (format t "~%Called handle-iota-form!~%: ~S~%" form)
  (vector-push-extend (make-rnode-instance 'iota-rnode rparent form)
                      (rnode-children rparent)))

(defun handle-call-form (rparent form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (vector-push-extend (make-rnode-instance 'call-rnode rparent form)
                      (rnode-children rparent)))

(defun handle-prim-call-form (rparent form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (vector-push-extend (make-rnode-instance 'prim-call-rnode rparent form)
                      (rnode-children rparent)))

(defun handle-prim-type-form (rparent form)
  ;; (format t "~%Called handle-type-form!~%: ~S~%" form)
  (vector-push-extend (make-rnode-instance 'prim-type-rnode rparent form)
                      (rnode-children rparent)))

(defun handle-prim-subforms (pn subforms)
  ;; (format t "~%Called handle-prim-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for fn = (case (car l)
                   ;; TODO : Handle other forms.
                   (:call   #'handle-prim-call-form)
                   (:|call| #'handle-prim-call-form)
                   (:type   #'handle-prim-type-form)
                   (:|type| #'handle-prim-type-form)
                   ;; (:meta   #'handle-prim-meta-form)
                   ;; (:|meta| #'handle-prim-meta-form)
                   ;; (:attr   #'handle-prim-attr-form)
                   ;; (:|attr| #'handle-prim-attr-form)
                   ;; (:rel    #'handle-prim-rel-form)
                   ;; (:|rel|  #'handle-prim-rel-form)
                   ;; (:ns     #'handle-prim-ns-form)
                   ;; (:|ns|   #'handle-prim-ns-form)
                   )
        do (if fn
               (funcall fn pn (cdr l))
               (unknown-form-error (car l) :debug))))

(defun handle-prim-form (rparent form)
  ;; (format t "~%Called handle-prim-form!~%: ~S~%" form)
  (let* ((pn (make-rnode-instance 'prim-rnode rparent form)))
    (vector-push-extend pn (rnode-children rparent))
    (handle-prim-subforms pn (list-rnode-children 'prim-rnode form))))

(defun handle-tree-form (rparent form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (vector-push-extend (make-rnode-instance 'tree-rnode rparent form)
                      (rnode-children rparent)))

(defun handle-meta-form (rparent form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)
  (vector-push-extend (make-rnode-instance 'meta-rnode rparent form)
                      (rnode-children rparent)))

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
            (let* ((rn (make-rnode-instance 'root-rnode nil usds-data)))
              (handle-data-subforms rn (list-rnode-children 'root-rnode usds-data))
              rn))))))
