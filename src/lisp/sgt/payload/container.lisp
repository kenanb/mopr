;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (container
            (:include payload)
            (:constructor nil)
            (:copier nil)))

(defstruct (root-container (:include container)))

(defstruct (group-container (:include container)))

(defstruct (prim-ns-container (:include container))
  (name-param (error "Unbound prim-ns-container: name-param")
   :read-only t
   :type base-string))
