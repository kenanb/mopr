;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-info)

(defstruct registry
  (value-types (make-instance 'value-types) :type value-types)
  (isa-schemas (make-instance 'isa-schemas) :type isa-schemas)
  (api-schemas (make-instance 'api-schemas) :type api-schemas))

;; Variable dynamically bound within the with-registry macro.
(defvar *registry* nil)

(defun populate-registry ()
  (populate-entries (registry-api-schemas *registry*))
  (populate-entries (registry-isa-schemas *registry*))
  (populate-entries (registry-value-types *registry*)))

(defun teardown-registry ()
  (teardown-entries (registry-api-schemas *registry*))
  (teardown-entries (registry-isa-schemas *registry*))
  (teardown-entries (registry-value-types *registry*)))

(defmacro with-registry ((&key supported-cases)
                         &body body)
  `(let ((*registry* (make-registry))
         (*registry-supported-cases* ,supported-cases))
     (prog2
         (populate-registry)
         (progn
           ,@body)
       (teardown-registry))))
