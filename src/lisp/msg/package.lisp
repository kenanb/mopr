;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-msg
  (:use #:cl)
  (:export

   ;; WORKSHOP SINGLETON

   #:acquire-ws
   #:release-ws
   #:ws-bound-p

   #:ws-id
   #:ws-location
   #:ws-projects
   #:ws-project-assignments
   #:ws-create-project
   #:ws-acquire-project
   #:ws-release-project

   ))
