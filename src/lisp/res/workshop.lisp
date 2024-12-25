;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defclass workshop ()
  ((id
    :type (simple-base-string 36)
    :initarg :id
    :initform (error "WORKSHOP cannot be initialized without an id!")
    :reader workshop-id
    :documentation "...")
   (projects
    :type list
    :initarg :projects
    :initform nil
    :accessor workshop-projects
    :documentation "An alist of project-id to PROJECT mappings.")
   (location
    :type pathname
    :initarg :location
    :initform (error "WORKSHOP cannot be initialized without a location!")
    :reader workshop-location
    :documentation "Absolute path of the workshop directory.")
   (project-assignments
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :accessor workshop-project-assignments
    :documentation "An alist of project ID to the ID of client
(currently assigned to project) mappings."))
  (:documentation "WORKSHOP

A workshop is mainly an abstraction over a filesystem directory that is a
container of projects.

Every workshop will be associated with a UUIDv7 at creation-time, so that the
underlying directory can be moved while maintaining stable addressing.

During startup, server process will be pointed at the workshop directory.

It is assumed that at any time, at most a single server is associated with a
specific workshop. This will be (mostly) guaranteed at the SINGLETON level.
"))

;; Utility

(defun validate-workshop-location (location)
  (unless (directory-exists-p location)
    (error "VALIDATE-WORKSHOP-LOCATION was given a non-existent directory."))
  (unless (file-exists-p (get-workshop-manifest-path location))
    (error "VALIDATE-WORKSHOP-LOCATION was given a directory that's missing a manifest.")))

;; Projects

(defun workshop-find-project-cons-by-id (ws proj-id)
  (assoc proj-id (workshop-projects)
         :test #'equal))

(defun workshop-find-project-cons-by-location (ws proj-location)
  (rassoc proj-location (workshop-projects ws)
          :key #'project-location
          :test #'equal))

(defun workshop-find-project-cons (ws lookup-type lookup-val)
  (case lookup-type
    (:id (workshop-find-project-cons-by-id ws lookup-val))
    (:location (workshop-find-project-cons-by-location
                ws
                (ensure-directory-pathname lookup-val)))
    (otherwise (error "Unknown project lookup type!"))))

(defun workshop-create-project (ws rel-project-directory)
  (unless (relative-pathname-p rel-project-directory)
    (error "WORKSHOP-CREATE-PROJECT requires a relative directory!"))
  (with-accessors ((location workshop-location)
                   (projects workshop-projects)) ws
    (validate-workshop-location location)
    (let* ((id (frugal-uuid:to-string (frugal-uuid:make-v7)))
           (proj (make-project
                  :location (ensure-directory-pathname rel-project-directory)))
           (proj-full-path (merge-pathnames* (project-location proj) location)))
      (when (workshop-find-project-cons-by-location ws (project-location proj))
        (error "This path was already registered!"))
      (ensure-all-directories-exist (list proj-full-path))
      (setf projects (acons id proj projects))
      (save-workshop-manifest-unchecked ws)
      nil)))

(defun workshop-acquire-project (ws lookup-type lookup-val session-id)
  (let* ((proj-cons (workshop-find-project-cons ws lookup-type lookup-val))
         (proj-id (car proj-cons))
         (existing (gethash proj-id (workshop-project-assignments ws))))
    (unless proj-id (error "Attempted to acquire unknown project!"))
    (if existing nil
        (prog1 proj-cons
          (setf (gethash proj-id (workshop-project-assignments ws)) session-id)))))

(defun workshop-release-project (ws lookup-type lookup-val session-id)
  (let* ((proj-id (car (workshop-find-project-cons ws lookup-type lookup-val)))
         (existing (gethash proj-id (workshop-project-assignments ws))))
    (unless proj-id (error "Attempted to release unknown project!"))
    (unless existing (error "Attempted to release project that was not acquired!"))
    (if (equal existing session-id)
        (remhash proj-id (workshop-project-assignments ws))
        (error "Attempted to release project acquired by another session!"))))

;; Lockfile Handling

;; WARNING: This is a very rudimentary lockfile approach.
;; It doesn't implement a reliable lock, just a simple one.

(defconstant +workshop-lockfile-filename+ "_mopr_workshop.lock")

(defconstant +workshop-lockfile-acquired+ "MOPR_WORKSHOP_LOCK_ACQUIRED")
(defconstant +workshop-lockfile-released+ "MOPR_WORKSHOP_LOCK_RELEASED")

(defun get-workshop-lockfile-path (location)
  (merge-pathnames* +workshop-lockfile-filename+ location))

(defun workshop-get-lockfile-state-unchecked (ws)
  (let* ((location (workshop-location ws))
         (lockfile (get-workshop-lockfile-path location))
         (lock-state-string (with-safe-io-syntax () (read-file-string lockfile))))
    (cond
      ((string= +workshop-lockfile-acquired+ lock-state-string) :acquired)
      ((string= +workshop-lockfile-released+ lock-state-string) :released)
      (t (error "Unexpected lockfile state value found!")))))

(defun workshop-set-lockfile-state-unchecked (ws state)
  (let* ((location (workshop-location ws))
         (lockfile (get-workshop-lockfile-path location))
         (state-string
           (case state
             (:acquired +workshop-lockfile-acquired+)
             (:released +workshop-lockfile-released+)
             (t (error "Unexpected lockfile state value requested!")))))
    (with-open-file (out lockfile :direction :output :if-exists :supersede)
      (with-safe-io-syntax () (princ state-string out))))
  state)

(defun workshop-set-lock-state-or-fail (ws requested)
  (validate-workshop-location (workshop-location ws))
  (when (eql requested (workshop-get-lockfile-state-unchecked ws))
    (error "Attempted to set the value to existing lockfile value!"))
  (workshop-set-lockfile-state-unchecked ws requested))

;; Manifest Handling

(defconstant +workshop-manifest-filename+ "_mopr_workshop.lisp")

(defun get-workshop-manifest-path (location)
  (merge-pathnames* +workshop-manifest-filename+ location))

(defun get-read-package ()
  (or (find-package "MOPR-USER")
      (error "Cannot find MOPR-USER package.~%")))

(defun load-workshop-manifest-unchecked (location
                                         &aux
                                           (manifest (get-workshop-manifest-path location))
                                           (read-pkg (get-read-package)))
  (with-open-file (in manifest)
    (with-safe-io-syntax (:package read-pkg)
      (apply #'make-instance 'workshop :location location (read in nil)))))

(defun load-workshop-manifest (directory
                               &aux
                                 (location (ensure-directory-pathname directory)))
  (validate-workshop-location location)
  (load-workshop-manifest-unchecked location))

(defun save-workshop-manifest-unchecked (ws
                                         &aux
                                           (location (workshop-location ws))
                                           (manifest (get-workshop-manifest-path location)))
  (with-open-file (out manifest :direction :output
                                :if-exists :supersede)
    (with-safe-io-syntax ()
      (prin1 (list
              :projects (workshop-projects ws)
              :id (workshop-id ws)) out))))

(defun save-workshop-manifest (ws &aux (location (workshop-location ws)))
  (validate-workshop-location location)
  (save-workshop-manifest-unchecked ws))

;; Workshop Setup

(defun setup-workshop (directory)
  "SETUP-WORKSHOP

Utility function to setup a new workshop at the existing, empty directory,
denoted by given absolute path (or absolute path namestring).

NOTES:

The DIRECTORY is treated as if it denotes a directory, even if it was
missing the '/' suffix.

This call won't try to lock the workshop directory it is about to populate.  It
is a utility function that is expected to be called very rarely. But since it
doesn't lock, it also does NOT return a WORKSHOP instance. Once this call
succeeds, caller should load the workshop using the normal APIs.

This call doesn't create the workshop directory itself, because:

- User creating a root directory in-advance for the workshop using standard
  tools helps ensure that the directory being provided is the intended location,
  and not due to misunderstanding about the implicit contract around pathnames.

- The API avoids the responsibility of deciding root directory permission.
"
  (unless (absolute-pathname-p directory)
    (error "MAKE-WORKSHOP requires an absolute directory!"))
  (let ((ws
          (make-instance 'workshop
                         :id (frugal-uuid:to-string (frugal-uuid:make-v7))
                         :location (ensure-directory-pathname directory))))
    (with-accessors ((location workshop-location)) ws

      (unless (directory-exists-p location)
        (error "SETUP-WORKSHOP requires the workshop directory to already exist."))
      (when (or (subdirectories location) (directory-files location))
        (error "SETUP-WORKSHOP requires the workshop directory to be initially empty."))
      (save-workshop-manifest-unchecked ws)
      (workshop-set-lockfile-state-unchecked ws :released)))
  nil)
