;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-org
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
                #:%desc-alist-assoc-by-desc
                #:%desc-alist-assoc-by-info
                #:%desc-alist-assoc-by-uuid)

  (:export

   ;; PNDESCRIPTOR
   #:pndescriptor-p
   #:pndescriptor
   #:pndescriptor-path

   ;; PROJECT
   #:make-project-info
   #:project-info-p
   #:project-info
   #:project-info-assets

   ;; WORKSHOP

   #:workshop-info
   #:workshop-info-projects
   #:workshop-info-sessions
   #:workshop-set-lock-state-or-fail
   #:load-workshop-manifest
   ;; #:save-workshop-manifest
   ;; #:save-project-manifest
   #:project-create-asset
   #:project-get-asset
   #:workshop-create-project
   #:workshop-acquire-project
   #:workshop-release-project
   #:setup-workshop

   ))
