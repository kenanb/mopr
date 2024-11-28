;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (directive
            (:include payload)
            (:constructor nil)
            (:copier nil)))

(defstruct (var-directive (:include directive))
  (name-param (error "Unbound var-directive: name-param")
   :read-only t
   :type symbol)
  (aux-form-param nil
   :read-only t
   :type list)
  (val-form-param nil
   :read-only t
   :type list))

(defstruct (each-directive (:include directive))
  (name-param (error "Unbound each-directive: name-param")
   :read-only t
   :type symbol)
  (keys-form-param nil
   :read-only t
   :type (or list symbol))
  (vals-form-param nil
   :read-only t
   :type list))

(defstruct (iota-directive (:include directive))
  (name-param (error "Unbound iota-directive: name-param")
   :read-only t
   :type symbol)
  (key-param (error "Unbound iota-directive: key-param")
   :read-only t
   :type symbol)
  (end-param (error "Unbound iota-directive: end-param")
   :read-only t
   :type integer)
  (start-param nil
   :read-only t
   :type (or null integer))
  (step-param nil
   :read-only t
   :type (or null integer)))

(defstruct (call-directive (:include directive))
  (aux-form-param (error "Unbound call-directive: aux-form-param")
   :read-only t
   :type (or list symbol))
  (body-form-param (error "Unbound call-directive: body-form-param")
   :read-only t
   :type list))

(defstruct (prim-call-directive (:include call-directive)))
