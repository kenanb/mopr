;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (statement
            (:include payload)
            (:constructor nil)
            (:copier nil)))

;; TODO : Unserializable and unrepresentable until the implementation is revised
;;        to only contain the information needed to generate the prop-info instance.
(defstruct (prim-schema-prop-statement (:include statement))
  (info-param (error "Unbound prim-schema-prop-statement param: info-param")
   :read-only t
   :type mopr-info:prop-info)
  (body-form-param (error "Unbound prim-schema-prop-statement param: body-form-param")
   :read-only t
   :type list))

(defstruct (prim-type-statement (:include statement))
  (name-param (error "Unbound prim-type-statement param: name-param")
   :read-only t
   :type (or keyword base-string)))

(defstruct (prim-attr-statement (:include statement))
  (name-param (error "Unbound prim-attr-statement param: name-param")
   :read-only t
   :type (or symbol base-string))
  (meta-form-param nil
   :read-only t
   :type list)
  (category-param (error "Unbound prim-attr-statement param: category-param")
   :read-only t
   :type keyword)
  (type-param (error "Unbound prim-attr-statement param: type-param")
   :read-only t
   :type keyword)
  (body-form-param (error "Unbound prim-attr-statement param: body-form-param")
   :read-only t
   :type list))

(defstruct (prim-rel-statement (:include statement))
  (name-param (error "Unbound prim-rel-statement param: name-param")
   :read-only t
   :type (or symbol base-string))
  (meta-form-param nil
   :read-only t
   :type list)
  (body-form-param (error "Unbound prim-rel-statement param: body-form-param")
   :read-only t
   :type list))

(defstruct (prim-statement (:include statement))
  (path-form-param (error "Unbound prim-statement param: path-form-param")
   :read-only t
   :type (or list symbol)))

(defstruct (tree-statement (:include statement))
  (body-form-param (error "Unbound tree-statement param: body-form-param")
   :read-only t
   :type list))

(defstruct (meta-statement (:include statement))
  (body-form-param (error "Unbound meta-statement param: body-form-param")
   :read-only t
   :type list))

(defstruct (prim-meta-statement (:include meta-statement)))
