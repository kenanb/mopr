;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-org)

(defconstant +project-manifest-filename+ "_mopr_project_manifest.lisp")

(defstruct (project-info (:include entity-info))
  "PROJECT-INFO

A project is mainly an abstraction over a filesystem directory that is a
container of assets.

At any time, a single client connected to the server (the server currently
assumed to be holding the lock to the workshop) is assumed to be working on a
specific project. This is expected to be tracked by the WORKSHOP instance.
"
  (assets nil
   :type list))

(defun get-project-manifest-path (ppath-full)
  (merge-pathnames* +project-manifest-filename+ ppath-full))

(defun validate-project-path (ppath-full)
  (unless (directory-exists-p ppath-full)
    (error "VALIDATE-PROJECT-PATH was given a non-existent directory."))
  (unless (file-exists-p (get-project-manifest-path ppath-full))
    (error "VALIDATE-PROJECT-PATH was given a directory that's missing a manifest.")))

(defun load-project-manifest-unchecked (pchain &aux (read-pkg (get-read-package)))
  (with-open-file (in (get-project-manifest-path (desc-chain-as-path pchain)))
    (with-manifest-io-syntax (:read-pkg read-pkg) (read in nil))))

(defun load-project-manifest (pchain)
  (validate-project-path (desc-chain-as-path pchain))
  (load-project-manifest-unchecked pchain))

(defun save-project-manifest-unchecked (ppath-full pinfo &aux (read-pkg (get-read-package)))
  (with-open-file (out (get-project-manifest-path ppath-full)
                       :direction :output
                       :if-exists :supersede)
    (with-manifest-io-syntax (:read-pkg read-pkg) (pprint pinfo out))))

(defun save-project-manifest (pchain pinfo
                              &aux (ppath-full (desc-chain-as-path pchain)))
  (validate-project-path ppath-full)
  (save-project-manifest-unchecked ppath-full pinfo))

(defun project-create-asset (pchain pinfo afile-rel
                             &rest ctor-kwargs &key &allow-other-keys)
  (unless (relative-pathname-p afile-rel)
    (error "PROJECT-CREATE-ASSET requires a relative directory!"))
  (with-accessors ((passets project-info-assets)) pinfo
    (validate-project-path (desc-chain-as-path pchain))
    (let* ((adesc (make-pndescriptor-for-file :asset afile-rel))
           (achain (mopr-uri:conc-desc-chains pchain (mopr-uri:make-desc-chain adesc)))
           (apath-full (desc-chain-as-path achain :file-expected-p t))
           (ainfo (apply #'make-asset-info ctor-kwargs)))
      (when (pndesc-alist-assoc passets :path (pndescriptor-path adesc))
        (error "This asset path was already registered!"))
      (ensure-all-directories-exist (list apath-full))
      (setf passets (acons adesc ainfo passets))
      (save-project-manifest pchain pinfo)
      (pndescriptor-uuid adesc))))

(defun project-get-asset (pinfo lookup-type lookup-val)
  (let ((sanitized-val (case lookup-type
                         (:path (or (file-pathname-p lookup-val)
                                    (error "Bad input for asset query!")))
                         (otherwise lookup-val))))
    (pndesc-alist-assoc (project-info-assets pinfo) lookup-type sanitized-val)))
