;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-reg)

;; Variable dynamically bound within the with-registry macro.
(defvar *registry-supported-cases* nil)

(declaim (inline write-mapping-with-case))

(defun write-mapping-with-case (table sym val
                                &aux
                                  (pkg (symbol-package sym)))

  (setf (gethash sym table) val)

  (when (member :upcase *registry-supported-cases*)
    (setf (gethash (intern (string-upcase sym) pkg) table) val))

  (when (member :downcase *registry-supported-cases*)
    (setf (gethash (intern (string-downcase sym) pkg) table) val)))
