;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-uri
  (:use #:cl)

  (:export

   ;; DESCRIPTOR
   #:new-uuid
   #:make-descriptor
   #:make-new-descriptor
   #:descriptor-p
   #:descriptor
   #:descriptor-role
   #:descriptor-uuid

   ;; DESC-CHAIN
   #:make-desc-chain
   #:desc-chain-p
   #:desc-chain
   #:desc-chain-contents
   #:conc-desc-chains
   #:desc-chain-as-uuid

   ;; DESC-ALIST
   #:desc-alist-assoc

   ;; Below are only exposed to use in DESCRIPTOR extensions.
   #:%desc-alist-assoc-by-desc
   #:%desc-alist-assoc-by-data
   #:%desc-alist-assoc-by-uuid

   ))
