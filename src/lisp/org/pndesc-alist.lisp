;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-org)

(defun %desc-alist-assoc-by-path (desc-alist val)
  (assoc val desc-alist
         :key #'pndescriptor-path
         :test #'equal))

(defun pndesc-alist-assoc (desc-alist lookup-type lookup-val)
  (let ((lookup-fn (case lookup-type
                     (:desc #'%desc-alist-assoc-by-desc)
                     (:info #'%desc-alist-assoc-by-info)
                     (:uuid #'%desc-alist-assoc-by-uuid)
                     (:path #'%desc-alist-assoc-by-path)
                     (otherwise (error "Unknown PNDESC-ALIST lookup type!")))))
    (funcall lookup-fn desc-alist lookup-val)))

(defun pndesc-alist-sanitizing-assoc (alist lookup-type lookup-val)
  (let* ((sanitized-val
           (case lookup-type
             ;; We shouldn't need to call NATIVE-NAMESTRING here.
             ;; Because this lookup is supposed to be used ONLY in the
             ;; context of relative paths, which are not expected to
             ;; refer to HOME anyway. But we validate for path being
             ;; simple, so that a possible failure to match due to
             ;; lookup path provided being complex is easy to debug.
             (:path (mopr-utl:validate-simple-path
                     (ensure-directory-pathname lookup-val)))
             (otherwise lookup-val))))
    (mopr-org:pndesc-alist-assoc alist lookup-type sanitized-val)))
