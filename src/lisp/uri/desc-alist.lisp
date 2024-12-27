;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-uri)

(defun %desc-alist-assoc-by-data (desc-alist val)
  (rassoc val desc-alist))

(defun %desc-alist-assoc-by-uuid (desc-alist val)
  (assoc val desc-alist
         :key #'descriptor-uuid
         :test #'equal))

(defun desc-alist-assoc (desc-alist lookup-type lookup-val)
  (let ((lookup-fn (case lookup-type
                     (:data #'%desc-alist-assoc-by-data)
                     (:uuid #'%desc-alist-assoc-by-uuid)
                     (otherwise (error "Unknown DESC-ALIST lookup type!")))))
    (funcall lookup-fn desc-alist lookup-val)))
