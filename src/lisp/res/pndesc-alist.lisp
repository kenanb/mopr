;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defun %descriptor-alist-assoc-by-path (desc-alist val)
  (assoc val desc-alist
         :key #'pndescriptor-path
         :test #'equal))

(defun pndescriptor-alist-assoc (desc-alist lookup-type lookup-val)
  (let ((lookup-fn (case lookup-type
                     (:data #'%descriptor-alist-assoc-by-data)
                     (:uuid #'%descriptor-alist-assoc-by-uuid)
                     (:path #'%descriptor-alist-assoc-by-path)
                     (otherwise (error "Unknown DESCRIPTOR-ALIST lookup type!")))))
    (funcall lookup-fn desc-alist lookup-val)))
