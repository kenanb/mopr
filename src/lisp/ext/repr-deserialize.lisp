;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr-deserialize
  (:import-from :mopr)
  (:import-from :mopr-ext/repr-shared
                #:with-layout-settings)
  (:import-from :mopr-ext/repr-rdata)
  (:import-from :mopr-ext/repr-rnode)
  (:use :cl)
  (:export
   #:deserialize-call-enabled))

(in-package :mopr-ext/repr-deserialize)

(defvar *debug-mode* t)

(defvar *enable-call* nil)

;;
;;; Utilities
;;

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

;;
;;; Form Handlers
;;

(defun handle-var-form (rparent form)
  ;; (format t "~%Called handle-var-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:var-rnode
                                     :rparent rparent
                                     :name-param (car form)
                                     :aux-form-param (cadr form)
                                     :val-form-param (cddr form))
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-each-form (rparent form)
  ;; (format t "~%Called handle-each-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:each-rnode
                                     :rparent rparent
                                     :name-param (car form)
                                     :keys-form-param (cadr form)
                                     :vals-form-param (cddr form))
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-iota-form (rparent form)
  ;; (format t "~%Called handle-iota-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:iota-rnode
                                     :rparent rparent
                                     :name-param (car form)
                                     :key-param (cadr form)
                                     :end-param (caddr form))
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-call-form (rparent form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:call-rnode
                                     :rparent rparent
                                     :aux-form-param (car form)
                                     :body-form-param (cdr form))
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-prim-form (rparent form)
  ;; (format t "~%Called handle-prim-form!~%: ~S~%" form)
  (let* ((pn (make-instance 'mopr-ext/repr-rnode:prim-rnode
                            :rparent rparent
                            :path-form-param (car form)
                            :meta-form-param (cadr form))))
    (vector-push-extend pn (mopr-ext/repr-rnode:rnode-children rparent))
    (loop for l in (cddr form)
          for i from 0
          for fn = (case (car l)
                     ;; TODO : Handle other forms.
                     (:call   #'handle-call-form)
                     (:|call| #'handle-call-form))
          do (if fn
                 (funcall fn pn (cdr l))
                 (unknown-form-error (car l) :debug)))))

(defun handle-tree-form (rparent form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:tree-rnode
                                     :rparent rparent
                                     :body-form-param form)
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-meta-form (rparent form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:meta-rnode
                                     :rparent rparent
                                     :body-form-param form)
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-data-subforms (rparent subforms)
  ;; (format t "~%Called handle-data-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for i from 0
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
            (let* ((n (make-instance 'mopr-ext/repr-rnode:root-rnode)))
              (handle-data-subforms n usds-data) n))))))
