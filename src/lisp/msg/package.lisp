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

   #:ws-descriptor
   #:ws-projects
   #:ws-sessions
   #:ws-create-project
   #:ws-acquire-project
   #:ws-release-project

   ))
