;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ops
  (:use #:cl)
  (:export

   ;; NODE-ID
   #:node-identifier
   #:node-identifier-id
   #:find-enode-by-id

   ;; OPTIONS
   #:payload-get-options
   #:enode-procedure-calculate-command-options
   #:enode-procedure-apply-command-option

   ;; COLLECT
   #:get-info-for-component
   #:collect-info-for-components

   ))
