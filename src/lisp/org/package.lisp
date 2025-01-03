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
   #:pndescriptor-uuid
   #:pndescriptor-path

   #:pndesc-alist-assoc
   #:pndesc-alist-sanitizing-assoc

   ;; ASSET
   #:make-asset-info
   #:asset-info-p
   #:asset-info
   #:asset-info-content-type

   ;; PROJECT
   #:make-project-info
   #:project-info-p
   #:project-info
   #:project-info-assets

   #:load-project-manifest

   ;; WORKSHOP
   #:make-workshop-info
   #:workshop-info-p
   #:workshop-info
   #:workshop-info-projects

   #:load-workshop-metadata
   #:load-workshop-manifest
   ;; #:save-workshop-manifest
   ;; #:save-project-manifest
   #:acquire-workshop
   #:release-workshop
   #:project-create-asset
   #:project-get-asset
   #:workshop-create-project
   #:setup-workshop

   ))
