;;;; package.lisp

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
