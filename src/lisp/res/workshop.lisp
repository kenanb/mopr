;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defclass workshop-info ()
  ((projects
    :type list
    :initarg :projects
    :initform nil
    :accessor workshop-info-projects
    :documentation "An alist of project DESCRIPTOR to PROJECT-INFO mappings.
Descriptor path is a workshop-relative path of the project directory.")
   (sessions
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :accessor workshop-info-sessions
    :documentation "An alist of project UUID to the ID of client
(currently assigned to project) mappings."))
  (:documentation "WORKSHOP-INFO

A workshop is mainly an abstraction over a filesystem directory that is a
container of projects.

During startup, server process will be pointed at the workshop directory.

It is assumed that at any time, at most a single server is associated with a
specific workshop. This will be (mostly) guaranteed at the SINGLETON level.
"))

;; Utility

(defconstant +workshop-manifest-filename+ "_mopr_workshop.lisp")

(defconstant +workshop-lockfile-filename+ "_mopr_workshop.lock")

(defun get-workshop-manifest-path (wpath)
  (merge-pathnames* +workshop-manifest-filename+ wpath))

(defun get-workshop-lockfile-path (wpath)
  (merge-pathnames* +workshop-lockfile-filename+ wpath))

(defun validate-workshop-path (wpath)
  (unless (directory-exists-p wpath)
    (error "VALIDATE-WORKSHOP-PATH was given a non-existent directory."))
  (unless (file-exists-p (get-workshop-lockfile-path wpath))
    (error "VALIDATE-WORKSHOP-PATH was given a directory that's missing a lockfile."))
  (unless (file-exists-p (get-workshop-manifest-path wpath))
    (error "VALIDATE-WORKSHOP-PATH was given a directory that's missing a manifest.")))

(defun get-read-package ()
  (or (find-package "MOPR-USER")
      (error "Cannot find MOPR-USER package.~%")))

(defmacro with-manifest-io-syntax ((&key read-pkg) &body body)
  `(with-standard-io-syntax
     (let ((*package* ,read-pkg)
           (*read-default-float-format* 'double-float)
           (*print-readably* t)
           (*read-eval* nil))
       ,@body)))

;; Projects

(defconstant +project-manifest-filename+ "_mopr_project.lisp")

(defun get-project-manifest-path (ppath-full)
  (merge-pathnames* +project-manifest-filename+ ppath-full))

(defun validate-project-path (ppath-full)
  (unless (directory-exists-p ppath-full)
    (error "VALIDATE-PROJECT-PATH was given a non-existent directory."))
  (unless (file-exists-p (get-project-manifest-path ppath-full))
    (error "VALIDATE-PROJECT-PATH was given a directory that's missing a manifest.")))

(defun load-project-manifest-unchecked (ppath-full &aux (read-pkg (get-read-package)))
  (with-open-file (in (get-project-manifest-path ppath-full))
    (with-manifest-io-syntax (:read-pkg read-pkg) (read in nil))))

(defun load-project-manifest (pchain
                              &aux (ppath-full (desc-chain-as-path pchain)))
  (validate-project-path ppath-full)
  (load-project-manifest-unchecked ppath-full))

(defun save-project-manifest-unchecked (ppath-full pinfo &aux (read-pkg (get-read-package)))
  (with-open-file (out (get-project-manifest-path ppath-full)
                       :direction :output
                       :if-exists :supersede)
    (with-manifest-io-syntax (:read-pkg read-pkg) (pprint pinfo out))))

(defun save-project-manifest (pchain pinfo
                              &aux (ppath-full (desc-chain-as-path pchain)))
  (validate-project-path ppath-full)
  (save-project-manifest-unchecked ppath-full pinfo))

(defun project-create-resource (pchain pinfo rfile-rel
                                &rest ctor-kwargs &key &allow-other-keys)
  (unless (relative-pathname-p rfile-rel)
    (error "PROJECT-CREATE-RESOURCE requires a relative directory!"))
  (with-accessors ((presources project-info-resources)) pinfo
    (validate-project-path (desc-chain-as-path pchain))
    (let* ((rdesc (make-pndescriptor-for-file :resource rfile-rel))
           (rchain (mopr-uri:conc-desc-chains pchain (mopr-uri:make-desc-chain rdesc)))
           (rpath-full (desc-chain-as-path rchain :file-expected-p t))
           (rinfo (apply #'make-resource-info ctor-kwargs)))
      (when (pndesc-alist-assoc presources :path (pndescriptor-path rdesc))
        (error "This resource path was already registered!"))
      (ensure-all-directories-exist (list rpath-full))
      (setf presources (acons rdesc rinfo presources))
      (save-project-manifest pchain pinfo)
      (pndescriptor-uuid rdesc))))

(defun project-get-resource (pinfo lookup-type lookup-val)
  (let ((sanitized-val (case lookup-type
                         (:path (or (file-pathname-p lookup-val)
                                    (error "Bad input for resource query!")))
                         (otherwise lookup-val))))
    (pndesc-alist-assoc (project-info-resources pinfo) lookup-type sanitized-val)))

(defun workshop-create-project (wcons pdir-rel
                                &rest ctor-kwargs &key &allow-other-keys
                                &aux (wdesc (car wcons)) (winfo (cdr wcons)))
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
    (validate-workshop-path (pndescriptor-path wdesc))
    (let* ((pdesc (make-pndescriptor-for-directory :project pdir-rel))
           (pchain (mopr-uri:make-desc-chain wdesc pdesc))
           (ppath-full (desc-chain-as-path pchain))
           (pinfo (apply #'make-project-info ctor-kwargs)))
      (when (pndesc-alist-assoc wprojects :path (pndescriptor-path pdesc))
        (error "This project path was already registered!"))
      (ensure-all-directories-exist (list ppath-full))
      (save-project-manifest-unchecked ppath-full pinfo)
      (setf wprojects (acons pdesc pinfo wprojects))
      (save-workshop-manifest-unchecked wcons)
      (pndescriptor-uuid pdesc))))

(defun workshop-acquire-project (wcons lookup-type lookup-val session-id
                                 &aux (winfo (cdr wcons)))
  (let* ((sanitized-val (case lookup-type
                          (:path (ensure-directory-pathname lookup-val))
                          (otherwise lookup-val)))
         (pcons (pndesc-alist-assoc (workshop-info-projects winfo) lookup-type sanitized-val)))
    (unless pcons (error "Attempted to acquire unknown project!"))
    (let* ((pdesc (car pcons))
           (puuid (pndescriptor-uuid pdesc))
           (current-id (gethash puuid (workshop-info-sessions winfo))))
      (if current-id
          (if (equal current-id session-id)
              (error "Attempted to acquire project already acquired by this session!")
              nil)
          (prog1 pcons
            (setf (gethash puuid (workshop-info-sessions winfo)) session-id))))))

(defun workshop-release-project (wcons lookup-type lookup-val session-id
                                 &aux (winfo (cdr wcons)))
  (let* ((sanitized-val (case lookup-type
                          (:path (ensure-directory-pathname lookup-val))
                          (otherwise lookup-val)))
         (pcons (pndesc-alist-assoc (workshop-info-projects winfo) lookup-type sanitized-val)))
    (unless pcons (error "Attempted to release unknown project!"))
    (let* ((pdesc (car pcons))
           (puuid (pndescriptor-uuid pdesc))
           (current-id (gethash puuid (workshop-info-sessions winfo))))
      (if current-id
          (if (equal current-id session-id)
              (remhash puuid (workshop-info-sessions winfo))
              (error "Attempted to release project acquired by another session!"))
          (error "Attempted to release project that was not acquired!")))))

;; Lockfile Handling

;; WARNING: This is a very rudimentary lockfile approach.
;; It doesn't implement a reliable lock, just a simple one.

(defconstant +workshop-lockfile-acquired-string+ "MOPR_WORKSHOP_LOCK_ACQUIRED")
(defconstant +workshop-lockfile-released-string+ "MOPR_WORKSHOP_LOCK_RELEASED")

(defun workshop-get-lockfile-state-unchecked (wcons &aux (wdesc (car wcons)))
  (let* ((wpath (pndescriptor-path wdesc))
         (lockfile-path (get-workshop-lockfile-path wpath))
         (lock-state-string (with-safe-io-syntax () (read-file-string lockfile-path))))
    (cond
      ((string= +workshop-lockfile-acquired-string+ lock-state-string) :acquired)
      ((string= +workshop-lockfile-released-string+ lock-state-string) :released)
      (t (error "Unexpected lockfile state value found!")))))

(defun workshop-set-lockfile-state-unchecked (wcons state &aux (wdesc (car wcons)))
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

(defun workshop-set-lock-state-or-fail (wcons requested &aux (wdesc (car wcons)))
  (validate-workshop-path (pndescriptor-path wdesc))
  (when (eql requested (workshop-get-lockfile-state-unchecked wcons))
    (error "Attempted to set the value to existing lockfile value!"))
  (workshop-set-lockfile-state-unchecked wcons requested))

;; Manifest Handling

(defun load-project-manifest-list-unchecked (wdesc pdesc-list)
  (flet ((load-pmanifest (pdesc)
           (cons pdesc (load-project-manifest
                        (mopr-uri:make-desc-chain wdesc pdesc)))))
    (mapcar #'load-pmanifest pdesc-list)))

(defun load-workshop-manifest-unchecked (wpath &aux (read-pkg (get-read-package)))
  (with-open-file (in (get-workshop-manifest-path wpath))
    (let* ((manifest (with-manifest-io-syntax (:read-pkg read-pkg) (read in nil)))
           (wuuid (getf manifest :uuid))
           (pdesc-list (getf manifest :projects))
           (wdesc (make-pndescriptor :role :workshop :uuid wuuid :path wpath))
           (wprojects (load-project-manifest-list-unchecked wdesc pdesc-list)))
      (cons wdesc (make-instance 'workshop-info :projects wprojects)))))

(defun load-workshop-manifest (wdir-abs
                               &aux (wpath (ensure-directory-pathname wdir-abs)))
  (validate-workshop-path wpath)
  (load-workshop-manifest-unchecked wpath))

(defun save-workshop-manifest-unchecked (wcons &aux (read-pkg (get-read-package))
                                                 (wdesc (car wcons)) (winfo (cdr wcons)))
  (with-open-file (out (get-workshop-manifest-path (pndescriptor-path wdesc))
                       :direction :output
                       :if-exists :supersede)
    (with-manifest-io-syntax (:read-pkg read-pkg)
      (pprint (list
               :projects (mapcar #'car (workshop-info-projects winfo))
               :uuid (pndescriptor-uuid wdesc)) out))))

(defun save-workshop-manifest (wcons &aux (wdesc (car wcons)))
  (validate-workshop-path (pndescriptor-path wdesc))
  (save-workshop-manifest-unchecked wcons))

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
         (wcons (cons wdesc (make-instance 'workshop-info)))
         (wpath (pndescriptor-path wdesc)))
    (unless (directory-exists-p wpath)
      (error "SETUP-WORKSHOP requires the workshop directory to already exist."))
    (when (or (subdirectories wpath) (directory-files wpath))
      (error "SETUP-WORKSHOP requires the workshop directory to be initially empty."))
    (save-workshop-manifest-unchecked wcons)
    (workshop-set-lockfile-state-unchecked wcons :released)
    (pndescriptor-uuid wdesc)))
