;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :mopr-sgt/plug)

;;;;;;;;;;
;;; Config
;;
;;

(declaim (type (or null configuration) *config*))

(defvar *config* nil
  "System object that holds configuration data.")

(defvar *call-table* nil)

(defclass configuration ()
  ((callables
    :initarg :callables
    :type list
    :initform nil
    :reader callables
    :documentation "Property list that will be looked up for callable definitions."))
  (:documentation "MOPR configuration."))

(defun configure ()
  "Loads the configuration."
  (let ((args (loop for pkg in '(:mopr-sgt/plug :mopr-sgt :mopr-user)
                    for cfg = (multiple-value-list (find-symbol "+CONFIGURATION+" pkg))
                    when (member (cadr cfg) '(:internal :external))
                      append (symbol-value (car cfg)))))
    (setf *config* (apply #'make-instance 'configuration
                          (loop with pl = nil
                                for (k v) in args
                                do (setf (getf pl k) (append v (getf pl k)))
                                finally (return pl))))))

(defun create-call-table (callable-plist table)
  (loop for (k v . rest) on callable-plist by #'cddr
        do (setf (gethash k table) v)))

(defun create-call-tables ()
  (create-call-table (callables *config*) *call-table*))

(defmacro with-configuration ((&key)
                              &body body)
  `(let* ((*config* nil)
          (*call-table* (make-hash-table)))
     (configure)
     (create-call-tables)
     ,@body))
