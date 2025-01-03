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

   ;; HANDLER
   #:request-handler-get
   #:request-handler-post

   ;; SESSION
   #:*messaging-session*
   #:messaging-session

   ;; DIRECT-CALLS
   #:bind-repr
   #:exec-repr
   #:init-repr
   #:term-repr
   #:populate-command-queue
   #:destruct-command-queue
   #:populate-command-options
   #:destruct-command-options
   #:apply-command-option

   ))
