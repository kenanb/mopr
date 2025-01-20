;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-org)

(defstruct (workshop-info (:include entity-info))
  "WORKSHOP-INFO

A workshop is mainly an abstraction over a filesystem directory that is a
container of projects.

During startup, MOPR backend process will be pointed at the workshop directory.

It is assumed that at any time, at most a single server is associated with a
specific workshop. This will be (mostly) guaranteed at the SINGLETON level.

PROJECTS: An alist of project DESCRIPTOR to NIL mappings.  Descriptor path is a
workshop-relative path of the project directory. We record descriptors
in (descriptor . nil) conses, to keep the query APIs generic. Corresponding
WORKSHOP class in MOPR-MSG then generates the fully populated version of this
alist.
"
  (projects nil
   :type list))

;; Utility

(defconstant +workshop-metadata-filename+ "_mopr_workshop_metadata.lisp")

(defconstant +workshop-manifest-filename+ "_mopr_workshop_manifest.lisp")

(defconstant +workshop-lockfile-filename+ "_mopr_workshop_lockfile")

(defun get-workshop-metadata-path (wpath)
  (merge-pathnames* +workshop-metadata-filename+ wpath))

(defun get-workshop-manifest-path (wpath)
  (merge-pathnames* +workshop-manifest-filename+ wpath))

(defun get-workshop-lockfile-path (wpath)
  (merge-pathnames* +workshop-lockfile-filename+ wpath))

(defun validate-workshop-path (wpath)
  (unless (directory-exists-p wpath)
    (error "VALIDATE-WORKSHOP-PATH was given a non-existent directory."))
  (unless (file-exists-p (get-workshop-lockfile-path wpath))
    (error "VALIDATE-WORKSHOP-PATH was given a directory that's missing the lockfile."))
  (unless (file-exists-p (get-workshop-metadata-path wpath))
    (error "VALIDATE-WORKSHOP-PATH was given a directory that's missing the metadata."))
  (unless (file-exists-p (get-workshop-manifest-path wpath))
    (error "VALIDATE-WORKSHOP-PATH was given a directory that's missing the manifest.")))

;; Projects

(defun workshop-create-project (wchain winfo pdir-rel
                                &rest ctor-kwargs &key &allow-other-keys)
  "WORKSHOP-CREATE-PROJECT

Utility function to setup a new project in workshop, at given workshop-relative
directory (or relative directory namestring).

NOTES:

The PDIR-REL is treated as if it denotes a directory, even if it was
missing the '/' suffix.

This call won't try to acquire (the lock for) the project it is populating,
from the workshop.  It is a utility function that is expected to be called
very rarely. But since it doesn't lock, it also does NOT return a
PROJECT-INFO instance. If needed, the caller should acquire the project
using WORKSHOP-ACQUIRE-PROJECT once this call succeeds.
"
  (unless (relative-pathname-p pdir-rel)
    (error "WORKSHOP-CREATE-PROJECT requires a relative directory!"))
  (with-accessors ((wprojects workshop-info-projects)) winfo
    (validate-workshop-path (desc-chain-as-path wchain))
    (let* ((pdesc (make-pndescriptor-for-directory :project pdir-rel))
           (pchain (mopr-uri:conc-desc-chains
                    wchain (mopr-uri:make-desc-chain pdesc)))
           (pinfo (apply #'make-project-info ctor-kwargs)))
      (when (pndesc-alist-assoc wprojects :path (pndescriptor-path pdesc))
        (error "This project path was already registered!"))
      (ensure-all-directories-exist (list (desc-chain-as-path pchain)))
      (save-project-manifest-unchecked pchain pinfo)
      (setf wprojects (acons pdesc nil wprojects))
      (save-workshop-manifest-unchecked wchain winfo)
      (pndescriptor-uuid pdesc))))

;; Lockfile Handling

;; WARNING: This is a very rudimentary lockfile approach.
;; It doesn't implement a reliable lock, just a simple one.

(defconstant +workshop-lockfile-acquired-string+ "MOPR_WORKSHOP_LOCK_ACQUIRED")
(defconstant +workshop-lockfile-released-string+ "MOPR_WORKSHOP_LOCK_RELEASED")

(defun get-workshop-lockfile-state-unchecked (wdesc)
  (let* ((wpath (pndescriptor-path wdesc))
         (lockfile-path (get-workshop-lockfile-path wpath))
         (lock-state-string (with-safe-io-syntax () (read-file-string lockfile-path))))
    (cond
      ((string= +workshop-lockfile-acquired-string+ lock-state-string) :acquired)
      ((string= +workshop-lockfile-released-string+ lock-state-string) :released)
      (t (error "Unexpected lockfile state value found!")))))

(defun set-workshop-lockfile-state-unchecked (wdesc state)
  (let* ((wpath (pndescriptor-path wdesc))
         (lockfile-path (get-workshop-lockfile-path wpath))
         (state-string
           (case state
             (:acquired +workshop-lockfile-acquired-string+)
             (:released +workshop-lockfile-released-string+)
             (t (error "Unexpected lockfile state value requested!")))))
    (with-open-file (out lockfile-path
                         :direction :output
                         :if-exists :supersede)
      (with-safe-io-syntax () (princ state-string out))))
  state)

(defun workshop-set-lock-state-or-fail (wdesc requested)
  (validate-workshop-path (pndescriptor-path wdesc))
  (when (eql requested (get-workshop-lockfile-state-unchecked wdesc))
    (error "Attempted to set the value to existing lockfile value!"))
  (set-workshop-lockfile-state-unchecked wdesc requested))

;; Metadata Handling

(defun load-workshop-metadata-unchecked (wpath &aux (read-pkg (get-read-package)))
  (with-open-file (in (get-workshop-metadata-path wpath))
    (let* ((metadata (with-manifest-io-syntax (:read-pkg read-pkg) (read in nil)))
           (wuuid (getf metadata :uuid)))
      (make-pndescriptor :role :workshop :uuid wuuid :path wpath))))

(defun load-workshop-metadata (wdir-abs
                               &aux
                                 (wdir-abs-native (native-namestring wdir-abs))
                                 (wpath (ensure-directory-pathname wdir-abs-native)))
  (mopr-utl:validate-simple-path wpath)
  (validate-workshop-path wpath)
  (load-workshop-metadata-unchecked wpath))

(defun save-workshop-metadata-unchecked (wdesc &aux (read-pkg (get-read-package)))
  (with-open-file (out (get-workshop-metadata-path (pndescriptor-path wdesc))
                       :direction :output
                       :if-exists :supersede)
    (with-manifest-io-syntax (:read-pkg read-pkg)
      (pprint (list :uuid (pndescriptor-uuid wdesc)) out))))

(defun save-workshop-metadata (wdesc)
  (validate-workshop-path (pndescriptor-path wdesc))
  (save-workshop-metadata-unchecked wdesc))

;; Manifest Handling

(defun load-workshop-manifest-unchecked (wchain &aux (read-pkg (get-read-package)))
  (with-open-file (in (get-workshop-manifest-path (desc-chain-as-path wchain)))
    (with-manifest-io-syntax (:read-pkg read-pkg) (read in nil))))

(defun load-workshop-manifest (wchain)
  (validate-workshop-path (desc-chain-as-path wchain))
  (load-workshop-manifest-unchecked wchain))

(defun save-workshop-manifest-unchecked (wchain winfo &aux (read-pkg (get-read-package)))
  (with-open-file (out (get-workshop-manifest-path (desc-chain-as-path wchain))
                       :direction :output
                       :if-exists :supersede)
    (with-manifest-io-syntax (:read-pkg read-pkg) (pprint winfo out))))

(defun save-workshop-manifest (wchain winfo)
  (validate-workshop-path (desc-chain-as-path wchain))
  (save-workshop-manifest-unchecked wchain winfo))

(defun acquire-workshop (wdesc)
  (progn
    (workshop-set-lock-state-or-fail wdesc :acquired)
    (load-workshop-manifest (mopr-uri:make-desc-chain wdesc))))

(defun release-workshop (wdesc)
  (prog1 nil
    (workshop-set-lock-state-or-fail wdesc :released)))

;; Workshop Setup

(defun setup-workshop (wdir-abs)
  "SETUP-WORKSHOP

Utility function to setup a new workshop at the existing, empty directory,
denoted by given absolute path (or absolute path namestring).

NOTES:

The WDIR-ABS is treated as if it denotes a directory, even if it was
missing the '/' suffix.

This call won't try to lock the workshop directory it is about to populate.  It
is a utility function that is expected to be called very rarely. But since it
doesn't lock, it also does NOT return a WORKSHOP instance. Once this call
succeeds, caller should load the workshop using the normal APIs.

This call doesn't create the workshop directory itself, because:

- User creating a root directory in-advance for the workshop using standard
  tools helps ensure that the directory being provided is the intended path,
  and not due to misunderstanding about the implicit contract around pathnames.

- The API avoids the responsibility of deciding root directory permission.
"
  (unless (absolute-pathname-p wdir-abs)
    (error "MAKE-WORKSHOP requires an absolute directory!"))
  (let* ((wdesc (make-pndescriptor-for-directory :workshop wdir-abs))
         (wchain (mopr-uri:make-desc-chain wdesc))
         (winfo (make-workshop-info))
         (wpath (pndescriptor-path wdesc)))
    (unless (directory-exists-p wpath)
      (error "SETUP-WORKSHOP requires the workshop directory to already exist."))
    (when (or (subdirectories wpath) (directory-files wpath))
      (error "SETUP-WORKSHOP requires the workshop directory to be initially empty."))
    (save-workshop-metadata-unchecked wdesc)
    (save-workshop-manifest-unchecked wchain winfo)
    (set-workshop-lockfile-state-unchecked wdesc :released)
    (pndescriptor-uuid wdesc)))
