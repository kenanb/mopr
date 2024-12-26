;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defstruct (descriptor
            (:constructor)
            (:constructor make-descriptor-for-directory
                (directory &aux
                             (uuid (frugal-uuid:to-string (frugal-uuid:make-v7)))
                             (path (ensure-directory-pathname directory)))))
  "DESCRIPTOR

A descriptor represents the means to unambiguously refer to a resource grouping.

Every grouping that has an associated descriptor will be associated with a
UUIDv7 at creation-time, so that the underlying directory can be moved while
maintaining stable addressing by the application.
"
  (uuid (error "DESCRIPTOR cannot be initialized without a uuid!")
   :type (simple-base-string 36)
   :read-only t)
  (path (error "DESCRIPTOR cannot be initialized without a path!")
   :type pathname
   :read-only t))

(defclass workshop ()
  ((descriptor
    :type descriptor
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

;; Projects

(defun workshop-find-project-cons-by-proj (ws proj)
  (rassoc proj (workshop-projects ws)))

(defun workshop-find-project-cons-by-uuid (ws puuid)
  (assoc puuid (workshop-projects ws)
         :key #'descriptor-uuid
         :test #'equal))

(defun workshop-find-project-cons-by-path (ws ppath)
  (assoc ppath (workshop-projects ws)
         :key #'descriptor-path
         :test #'equal))

(defun workshop-find-project-cons (ws lookup-type lookup-val)
  (case lookup-type
    (:uuid (workshop-find-project-cons-by-uuid ws lookup-val))
    (:path (workshop-find-project-cons-by-path ws (ensure-directory-pathname lookup-val)))
    (otherwise (error "Unknown project lookup type!"))))

(defun workshop-create-project (ws pdir-rel)
  (unless (relative-pathname-p pdir-rel)
    (error "WORKSHOP-CREATE-PROJECT requires a relative directory!"))
  (with-accessors ((wdesc workshop-descriptor)
                   (wprojects workshop-projects)) ws
    (validate-workshop-path (descriptor-path wdesc))
    (let* ((pdesc (make-descriptor-for-directory pdir-rel))
           (ppath-full (merge-pathnames* (descriptor-path pdesc)
                                         (descriptor-path wdesc))))
      (when (workshop-find-project-cons-by-path ws (descriptor-path pdesc))
        (error "This path was already registered!"))
      (ensure-all-directories-exist (list ppath-full))
      ;; TODO : Add support for the actual project description.
      (setf wprojects (acons pdesc nil wprojects))
      (save-workshop-manifest-unchecked ws)
      nil)))

(defun workshop-acquire-project (ws lookup-type lookup-val session-id)
  (let ((pcons (workshop-find-project-cons ws lookup-type lookup-val)))
    (unless pcons (error "Attempted to acquire unknown project!"))
    (let* ((pdesc (car pcons))
           (puuid (descriptor-uuid pdesc))
           (current-id (gethash puuid (workshop-project-assignments ws))))
      (if current-id
          (if (equal current-id session-id)
              (error "Attempted to acquire project already acquired by this session!")
              nil)
          (prog1 pcons
            (setf (gethash puuid (workshop-project-assignments ws)) session-id))))))

(defun workshop-release-project (ws lookup-type lookup-val session-id)
  (let ((pcons (workshop-find-project-cons ws lookup-type lookup-val)))
    (unless pcons (error "Attempted to release unknown project!"))
    (let* ((pdesc (car pcons))
           (puuid (descriptor-uuid pdesc))
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
  (let* ((wpath (descriptor-path (workshop-descriptor ws)))
         (lockfile-path (get-workshop-lockfile-path wpath))
         (lock-state-string (with-safe-io-syntax () (read-file-string lockfile-path))))
    (cond
      ((string= +workshop-lockfile-acquired-string+ lock-state-string) :acquired)
      ((string= +workshop-lockfile-released-string+ lock-state-string) :released)
      (t (error "Unexpected lockfile state value found!")))))

(defun workshop-set-lockfile-state-unchecked (ws state)
  (let* ((wpath (descriptor-path (workshop-descriptor ws)))
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
  (validate-workshop-path (descriptor-path (workshop-descriptor ws)))
  (when (eql requested (workshop-get-lockfile-state-unchecked ws))
    (error "Attempted to set the value to existing lockfile value!"))
  (workshop-set-lockfile-state-unchecked ws requested))

;; Manifest Handling

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

(defun load-workshop-manifest-unchecked (wpath &aux (read-pkg (get-read-package)))
  (with-open-file (in (get-workshop-manifest-path wpath))
    (let* ((manifest (with-manifest-io-syntax (:read-pkg read-pkg) (read in nil)))
           (wuuid (getf manifest :uuid))
           (wdesc (make-descriptor :uuid wuuid :path wpath))
           ;; TODO : Add support for the actual project description.
           (wprojects (mapcar #'list (getf manifest :projects))))
      (make-instance 'workshop :descriptor wdesc :projects wprojects))))

(defun load-workshop-manifest (directory
                               &aux (wpath (ensure-directory-pathname directory)))
  (validate-workshop-path wpath)
  (load-workshop-manifest-unchecked wpath))

(defun save-workshop-manifest-unchecked (ws &aux (wdesc (workshop-descriptor ws))
                                              (read-pkg (get-read-package)))
  (with-open-file (out (get-workshop-manifest-path (descriptor-path wdesc))
                       :direction :output
                       :if-exists :supersede)
    (with-manifest-io-syntax (:read-pkg read-pkg)
      (pprint (list
               :projects (mapcar #'car (workshop-projects ws))
               :uuid (descriptor-uuid (workshop-descriptor ws))) out))))

(defun save-workshop-manifest (ws)
  (validate-workshop-path (descriptor-path (workshop-descriptor ws)))
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
  (let* ((wdesc (make-descriptor-for-directory wdir-abs))
         (ws (make-instance 'workshop :descriptor wdesc))
         (wpath (descriptor-path wdesc)))
    (unless (directory-exists-p wpath)
      (error "SETUP-WORKSHOP requires the workshop directory to already exist."))
    (when (or (subdirectories wpath) (directory-files wpath))
      (error "SETUP-WORKSHOP requires the workshop directory to be initially empty."))
    (save-workshop-manifest-unchecked ws)
    (workshop-set-lockfile-state-unchecked ws :released))
  nil)
