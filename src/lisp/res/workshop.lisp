;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defclass workshop ()
  ((descriptor
    :type pndescriptor
    :initarg :descriptor
    :initform (error "WORKSHOP cannot be initialized without a descriptor!")
    :reader workshop-descriptor
    :documentation "Descriptor path is the absolute path of the workshop directory.")
   (projects
    :type list
    :initarg :projects
    :initform nil
    :accessor workshop-projects
    :documentation "An alist of project DESCRIPTOR to PROJECT mappings.
Descriptor path is a workshop-relative path of the project directory.")
   (project-assignments
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :accessor workshop-project-assignments
    :documentation "An alist of project UUID to the ID of client
(currently assigned to project) mappings."))
  (:documentation "WORKSHOP

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

(defun load-project-manifest (wdesc pdesc
                              &aux (ppath-full (desc-chain-as-path
                                                (mopr-uri:make-desc-chain wdesc pdesc))))
  (validate-project-path ppath-full)
  (load-project-manifest-unchecked ppath-full))

(defun save-project-manifest-unchecked (ppath-full proj &aux (read-pkg (get-read-package)))
  (with-open-file (out (get-project-manifest-path ppath-full)
                       :direction :output
                       :if-exists :supersede)
    (with-manifest-io-syntax (:read-pkg read-pkg) (pprint proj out))))

(defun save-project-manifest (wdesc pdesc proj
                              &aux (ppath-full (desc-chain-as-path
                                                (mopr-uri:make-desc-chain wdesc pdesc))))
  (validate-project-path ppath-full)
  (save-project-manifest-unchecked ppath-full proj))

(defun project-create-resource (wdesc pdesc proj rfile-rel
                                &rest ctor-kwargs &key &allow-other-keys)
  (unless (relative-pathname-p rfile-rel)
    (error "PROJECT-CREATE-RESOURCE requires a relative directory!"))
  (validate-project-path (desc-chain-as-path (mopr-uri:make-desc-chain wdesc pdesc)))
  (let* ((rdesc (make-pndescriptor-for-file :resource rfile-rel))
         (rpath-full (desc-chain-as-path (mopr-uri:make-desc-chain wdesc pdesc rdesc)
                                         :file-expected-p t))
         (res (apply #'make-resource ctor-kwargs)))
    (when (pndesc-alist-assoc (project-resources proj)
                              :path (pndescriptor-path rdesc))
      (error "This resource path was already registered!"))
    (ensure-all-directories-exist (list rpath-full))
    (setf (project-resources proj) (acons rdesc res (project-resources proj)))
    (save-project-manifest wdesc pdesc proj)
    (pndescriptor-uuid rdesc)))

(defun project-get-resource (proj lookup-type lookup-val)
  (let ((sanitized-val (case lookup-type
                         (:path (or (file-pathname-p lookup-val)
                                    (error "Bad input for resource query!")))
                         (otherwise lookup-val))))
    (pndesc-alist-assoc (project-resources proj) lookup-type sanitized-val)))

(defun workshop-create-project (ws pdir-rel
                                &rest ctor-kwargs &key &allow-other-keys)
  "WORKSHOP-CREATE-PROJECT

Utility function to setup a new project in workshop, at given workshop-relative
directory (or relative directory namestring).

NOTES:

The PDIR-REL is treated as if it denotes a directory, even if it was
missing the '/' suffix.

This call won't try to acquire (the lock for) the project it is populating,
from the workshop.  It is a utility function that is expected to be called
very rarely. But since it doesn't lock, it also does NOT return a PROJECT
instance. If needed, the caller should acquire the project using
WORKSHOP-ACQUIRE-PROJECT once this call succeeds.
"
  (unless (relative-pathname-p pdir-rel)
    (error "WORKSHOP-CREATE-PROJECT requires a relative directory!"))
  (with-accessors ((wdesc workshop-descriptor)
                   (wprojects workshop-projects)) ws
    (validate-workshop-path (pndescriptor-path wdesc))
    (let* ((pdesc (make-pndescriptor-for-directory :project pdir-rel))
           (ppath-full (desc-chain-as-path (mopr-uri:make-desc-chain wdesc pdesc)))
           (proj (apply #'make-project ctor-kwargs)))
      (when (pndesc-alist-assoc (workshop-projects ws)
                                :path (pndescriptor-path pdesc))
        (error "This project path was already registered!"))
      (ensure-all-directories-exist (list ppath-full))
      (save-project-manifest-unchecked ppath-full proj)
      (setf wprojects (acons pdesc proj wprojects))
      (save-workshop-manifest-unchecked ws)
      (pndescriptor-uuid pdesc))))

(defun workshop-acquire-project (ws lookup-type lookup-val session-id)
  (let* ((sanitized-val (case lookup-type
                          (:path (ensure-directory-pathname lookup-val))
                          (otherwise lookup-val)))
         (pcons (pndesc-alist-assoc (workshop-projects ws) lookup-type sanitized-val)))
    (unless pcons (error "Attempted to acquire unknown project!"))
    (let* ((pdesc (car pcons))
           (puuid (pndescriptor-uuid pdesc))
           (current-id (gethash puuid (workshop-project-assignments ws))))
      (if current-id
          (if (equal current-id session-id)
              (error "Attempted to acquire project already acquired by this session!")
              nil)
          (prog1 pcons
            (setf (gethash puuid (workshop-project-assignments ws)) session-id))))))

(defun workshop-release-project (ws lookup-type lookup-val session-id)
  (let* ((sanitized-val (case lookup-type
                          (:path (ensure-directory-pathname lookup-val))
                          (otherwise lookup-val)))
         (pcons (pndesc-alist-assoc (workshop-projects ws) lookup-type sanitized-val)))
    (unless pcons (error "Attempted to release unknown project!"))
    (let* ((pdesc (car pcons))
           (puuid (pndescriptor-uuid pdesc))
           (current-id (gethash puuid (workshop-project-assignments ws))))
      (if current-id
          (if (equal current-id session-id)
              (remhash puuid (workshop-project-assignments ws))
              (error "Attempted to release project acquired by another session!"))
          (error "Attempted to release project that was not acquired!")))))

;; Lockfile Handling

;; WARNING: This is a very rudimentary lockfile approach.
;; It doesn't implement a reliable lock, just a simple one.

(defconstant +workshop-lockfile-acquired-string+ "MOPR_WORKSHOP_LOCK_ACQUIRED")
(defconstant +workshop-lockfile-released-string+ "MOPR_WORKSHOP_LOCK_RELEASED")

(defun workshop-get-lockfile-state-unchecked (ws)
  (let* ((wpath (pndescriptor-path (workshop-descriptor ws)))
         (lockfile-path (get-workshop-lockfile-path wpath))
         (lock-state-string (with-safe-io-syntax () (read-file-string lockfile-path))))
    (cond
      ((string= +workshop-lockfile-acquired-string+ lock-state-string) :acquired)
      ((string= +workshop-lockfile-released-string+ lock-state-string) :released)
      (t (error "Unexpected lockfile state value found!")))))

(defun workshop-set-lockfile-state-unchecked (ws state)
  (let* ((wpath (pndescriptor-path (workshop-descriptor ws)))
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

(defun workshop-set-lock-state-or-fail (ws requested)
  (validate-workshop-path (pndescriptor-path (workshop-descriptor ws)))
  (when (eql requested (workshop-get-lockfile-state-unchecked ws))
    (error "Attempted to set the value to existing lockfile value!"))
  (workshop-set-lockfile-state-unchecked ws requested))

;; Manifest Handling

(defun load-workshop-manifest-unchecked (wpath &aux (read-pkg (get-read-package)))
  (with-open-file (in (get-workshop-manifest-path wpath))
    (let* ((manifest (with-manifest-io-syntax (:read-pkg read-pkg) (read in nil)))
           (wuuid (getf manifest :uuid))
           (wdesc (make-pndescriptor :role :workshop :uuid wuuid :path wpath))
           (wprojects (mapcar (lambda (pdesc)
                                (cons pdesc
                                      (load-project-manifest wdesc pdesc)))
                              (getf manifest :projects))))
      (make-instance 'workshop :descriptor wdesc :projects wprojects))))

(defun load-workshop-manifest (directory
                               &aux (wpath (ensure-directory-pathname directory)))
  (validate-workshop-path wpath)
  (load-workshop-manifest-unchecked wpath))

(defun save-workshop-manifest-unchecked (ws &aux (wdesc (workshop-descriptor ws))
                                              (read-pkg (get-read-package)))
  (with-open-file (out (get-workshop-manifest-path (pndescriptor-path wdesc))
                       :direction :output
                       :if-exists :supersede)
    (with-manifest-io-syntax (:read-pkg read-pkg)
      (pprint (list
               :projects (mapcar #'car (workshop-projects ws))
               :uuid (pndescriptor-uuid (workshop-descriptor ws))) out))))

(defun save-workshop-manifest (ws)
  (validate-workshop-path (pndescriptor-path (workshop-descriptor ws)))
  (save-workshop-manifest-unchecked ws))

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
         (ws (make-instance 'workshop :descriptor wdesc))
         (wpath (pndescriptor-path wdesc)))
    (unless (directory-exists-p wpath)
      (error "SETUP-WORKSHOP requires the workshop directory to already exist."))
    (when (or (subdirectories wpath) (directory-files wpath))
      (error "SETUP-WORKSHOP requires the workshop directory to be initially empty."))
    (save-workshop-manifest-unchecked ws)
    (workshop-set-lockfile-state-unchecked ws :released)
    (pndescriptor-uuid wdesc)))
