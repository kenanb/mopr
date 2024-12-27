;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-uri)

(defun %descriptor-alist-assoc-by-data (desc-alist val)
  (rassoc val desc-alist))

(defun %descriptor-alist-assoc-by-uuid (desc-alist val)
  (assoc val desc-alist
         :key #'descriptor-uuid
         :test #'equal))

(defun descriptor-alist-assoc (desc-alist lookup-type lookup-val)
  (let ((lookup-fn (case lookup-type
                     (:data #'%descriptor-alist-assoc-by-data)
                     (:uuid #'%descriptor-alist-assoc-by-uuid)
                     (otherwise (error "Unknown DESCRIPTOR-ALIST lookup type!")))))
    (funcall lookup-fn desc-alist lookup-val)))
