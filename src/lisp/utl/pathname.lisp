;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-utl)

(defun validate-simple-path (path)
  (when (wild-pathname-p path)
    (error "Pathnames that include wild components are unsupported!"))
  (let ((dir-component (pathname-directory path)))
    (etypecase dir-component
      (keyword (error "KEYWORD typed directory component is unsupported!"))
      (string (error "STRING directory component is unsupported!"))
      (list (when (notevery #'stringp (cdr dir-component))
              (error "Directory component with special markers is unsupported!")))))
  path)
