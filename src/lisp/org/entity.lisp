;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-org)

(defstruct entity-info
  "ENTITY-INFO

Base class of workshop content.
"
  (created-time (get-universal-time)
   :type integer
   :read-only t)
  (description ""
   :type string))

(defun get-read-package ()
  (or (find-package "MOPR-USER")
      (error "Cannot find MOPR-USER package.~%")))

(defmacro with-manifest-io-syntax ((&key read-pkg) &body body)
  `(with-standard-io-syntax
     (let ((*package* ,read-pkg)
           (*read-default-float-format* 'double-float)
           (*print-readably* t)
           (*read-eval* nil))
       ,@body)))
