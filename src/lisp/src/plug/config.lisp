;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :mopr-plug)

;;;;;;;;;;
;;; Config
;;
;;

(declaim (type (or null configuration) *config*))

(defvar *config* nil
  "System object that holds configuration data.")

(defvar *data-call-table* nil)

(defvar *prim-call-table* nil)

(defclass configuration ()
  ((prim-callables
    :initarg :prim-callables
    :type list
    :initform nil
    :reader prim-callables
    :documentation "Property list that will be looked up for prim callable definitions.")
   (data-callables
    :initarg :data-callables
    :type list
    :initform nil
    :reader data-callables
    :documentation "Property list that will be looked up for data callable definitions."))
  (:documentation "MOPR configuration."))

(defun configure (&aux (config-var
                        (multiple-value-list (find-symbol "+CONFIGURATION+" :mopr-user))))
  "Loads the configuration."
  (when (member (cadr config-var) '(:internal :external))
    (let* ((args (symbol-value (car config-var)))
           (key-args
             (loop with pl = nil
                   for (k v) in args
                   do (setf (getf pl k) (append v (getf pl k)))
                   finally (return pl))))
      (when (listp key-args)
        (setf *config* (apply #'make-instance 'configuration key-args))))))

(defun create-prim-call-table (table)
  (loop for (k v . rest) on (prim-callables *config*) by #'cddr
        do (setf (gethash k table) v)))

(defun create-data-call-table (table)
  (loop for (k v . rest) on (data-callables *config*) by #'cddr
        do (setf (gethash k table) v)))

(defmacro with-configuration ((&key)
                              &body body)
  `(let* ((*config* nil)
          (*data-call-table* (make-hash-table))
          (*prim-call-table* (make-hash-table)))
     (configure)
     (create-data-call-table *data-call-table*)
     (create-prim-call-table *prim-call-table*)
     ,@body))
