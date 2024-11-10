;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/enode-copy
  (:import-from :mopr)
  (:use :mopr-sgt)
  (:use :cl)
  (:export
   #:copy-enode-instance))

(in-package :mopr-ext/enode-copy)

;;
;;; MAIN API
;;

(defgeneric enode-copy-params (node)
  (:documentation "Get the plist that represents the initargs
of a new instance that is a shallow copy."))

(defun copy-enode-instance (orig-node &optional parent)
  (apply #'make-instance (type-of orig-node)
         :parent parent
         (enode-copy-params orig-node)))

;;
;;; ENODE-COPY-PARAMS IMPLEMENTATIONS
;;

(defmethod enode-copy-params ((n root-enode))
  (declare (ignore n))
  nil)

(defmethod enode-copy-params ((n group-enode))
  (declare (ignore n))
  nil)

(defmethod enode-copy-params ((n var-enode))
  (list :name-param (var-enode-name-param n)
        :aux-form-param (var-enode-aux-form-param n)
        :val-form-param (var-enode-val-form-param n)))

(defmethod enode-copy-params ((n each-enode))
  (list :name-param (each-enode-name-param n)
        :keys-form-param (each-enode-keys-form-param n)
        :vals-form-param (each-enode-vals-form-param n)))

(defmethod enode-copy-params ((n iota-enode))
  (list :name-param (iota-enode-name-param n)
        :key-param (iota-enode-key-param n)
        :end-param (iota-enode-end-param n)
        :start-param (iota-enode-start-param n)
        :step-param (iota-enode-step-param n)))

(defmethod enode-copy-params ((n call-enode))
  (list :aux-form-param (call-enode-aux-form-param n)
        :body-form-param (call-enode-body-form-param n)))

(defmethod enode-copy-params ((n prim-schema-prop-enode))
  (list :info-param (prim-schema-prop-enode-info-param n)
        :body-form-param (prim-schema-prop-enode-body-form-param n)))

(defmethod enode-copy-params ((n prim-type-enode))
  (list :name-param (prim-type-enode-name-param n)))

(defmethod enode-copy-params ((n prim-attr-enode))
  (list :name-param (prim-attr-enode-name-param n)
        :meta-form-param (prim-attr-enode-meta-form-param n)
        :category-param (prim-attr-enode-category-param n)
        :type-param (prim-attr-enode-type-param n)
        :body-form-param (prim-attr-enode-body-form-param n)))

(defmethod enode-copy-params ((n prim-rel-enode))
  (list :name-param (prim-rel-enode-name-param n)
        :meta-form-param (prim-rel-enode-meta-form-param n)
        :body-form-param (prim-rel-enode-body-form-param n)))

(defmethod enode-copy-params ((n prim-ns-enode))
  (list :name-param (prim-ns-enode-name-param n)))

(defmethod enode-copy-params ((n prim-enode))
  (list :path-form-param (prim-enode-path-form-param n)))

(defmethod enode-copy-params ((n tree-enode))
  (list :body-form-param (tree-enode-body-form-param n)))

(defmethod enode-copy-params ((n meta-enode))
  (list :body-form-param (meta-enode-body-form-param n)))
