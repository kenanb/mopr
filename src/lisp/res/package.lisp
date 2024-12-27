;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-res
  (:use #:cl)
  (:import-from :uiop/stream
                #:read-file-string
                #:with-safe-io-syntax)
  (:import-from :uiop/filesystem
                #:file-exists-p
                #:directory-exists-p
                #:directory-files
                #:subdirectories
                #:ensure-all-directories-exist)
  (:import-from :uiop/pathname
                #:file-pathname-p
                #:directory-pathname-p
                #:absolute-pathname-p
                #:relative-pathname-p
                #:ensure-directory-pathname
                #:pathname-directory-pathname
                #:merge-pathnames*
                #:subpathname
                #:subpathname*
                #:unix-namestring
                #:parse-unix-namestring)

  (:import-from :mopr-uri
                #:%descriptor-alist-assoc-by-data
                #:%descriptor-alist-assoc-by-uuid)

  (:export

   ;; PROJECT
   #:make-project
   #:project-p
   #:project
   #:project-resources

   ;; WORKSHOP

   #:workshop
   #:workshop-descriptor
   #:workshop-projects
   #:workshop-project-assignments
   #:workshop-set-lock-state-or-fail
   #:load-workshop-manifest
   ;; #:save-workshop-manifest
   ;; #:save-project-manifest
   #:project-create-resource
   #:project-get-resource
   #:workshop-create-project
   #:workshop-acquire-project
   #:workshop-release-project
   #:setup-workshop

   ))
