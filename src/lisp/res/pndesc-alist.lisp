;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defun %desc-alist-assoc-by-path (desc-alist val)
  (assoc val desc-alist
         :key #'pndescriptor-path
         :test #'equal))

(defun pndesc-alist-assoc (desc-alist lookup-type lookup-val)
  (let ((lookup-fn (case lookup-type
                     (:data #'%desc-alist-assoc-by-data)
                     (:uuid #'%desc-alist-assoc-by-uuid)
                     (:path #'%desc-alist-assoc-by-path)
                     (otherwise (error "Unknown PNDESC-ALIST lookup type!")))))
    (funcall lookup-fn desc-alist lookup-val)))
