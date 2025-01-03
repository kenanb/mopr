;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defclass workshop ()
  ((descriptor
    :type mopr-org:pndescriptor
    :initarg :descriptor
    :initform (error "WORKSHOP cannot be initialized without a descriptor.")
    :reader workshop-descriptor
    :documentation "Descriptor holding the absolute path to associated workshop.")
   (information
    :type mopr-org:workshop-info
    :initarg :information
    :initform (error "WORKSHOP cannot be initialized without information.")
    :reader workshop-information
    :documentation "WORKSHOP-INFO instance that represents persistent information.")
   (projects
    :type list
    :initarg :projects
    :initform nil
    :accessor workshop-projects
    :documentation "An alist of project DESCRIPTOR to PROJECT-INFO mappings.
Descriptor path is a workshop-relative path of the project directory.")
   (sessions
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :accessor workshop-sessions
    :documentation "An hash-table of project UUID to the ID of client (currently
assigned to project) mappings."))
  (:documentation "WORKSHOP

This class wraps WORKSHOP-INFO instance, and augments it with runtime information.

During startup, MOPR backend process will be pointed at the workshop directory.

It is assumed that at any time, at most a single server is associated with a
specific workshop. This will be (mostly) guaranteed at the SINGLETON level.
"))

(defun workshop-acquire-project (ws lookup-type lookup-val session-id)
  (let ((pcons (mopr-org:pndesc-alist-sanitizing-assoc (workshop-projects ws)
                                                       lookup-type lookup-val)))
    (unless pcons (error "Attempted to acquire unknown project!"))
    (let* ((pdesc (car pcons))
           (puuid (mopr-org:pndescriptor-uuid pdesc))
           (current-id (gethash puuid (workshop-sessions ws))))
      (if current-id
          (if (equal current-id session-id)
              (error "Attempted to acquire project already acquired by this session!")
              nil)
          (prog1 pcons
            (setf (gethash puuid (workshop-sessions ws)) session-id))))))

(defun workshop-release-project (ws lookup-type lookup-val session-id)
  (let ((pcons (mopr-org:pndesc-alist-sanitizing-assoc (workshop-projects ws)
                                                       lookup-type lookup-val)))
    (unless pcons (error "Attempted to release unknown project!"))
    (let* ((pdesc (car pcons))
           (puuid (mopr-org:pndescriptor-uuid pdesc))
           (current-id (gethash puuid (workshop-sessions ws))))
      (if current-id
          (if (equal current-id session-id)
              (remhash puuid (workshop-sessions ws))
              (error "Attempted to release project acquired by another session!"))
          (error "Attempted to release project that was not acquired!")))))

(defun ingest-project (wchain pcons &aux (pdesc (car pcons)))
  (cons pdesc (mopr-org:load-project-manifest
               (mopr-uri:conc-desc-chains
                wchain (mopr-uri:make-desc-chain pdesc)))))

(defun ingest-projects (wchain pconses)
  (mapcar (lambda (pcons) (ingest-project wchain pcons))
          pconses))

(defvar *workshop* nil)

(defvar *workshop-lock* (bt:make-lock))

(defun acquire-ws (wdir-abs &aux (wdesc (mopr-org:load-workshop-metadata wdir-abs))
                              (winfo (mopr-org:acquire-workshop wdesc)))
  (setf *workshop* (make-instance 'workshop
                                  :descriptor wdesc
                                  :information winfo
                                  :projects (ingest-projects
                                             (mopr-uri:make-desc-chain wdesc)
                                             (mopr-org:workshop-info-projects winfo)))))

(defun release-ws (&aux (wdesc (workshop-descriptor *workshop*)))
  (mopr-org:release-workshop wdesc)
  (setf *workshop* nil))

(defun ws-bound-p ()
  (if *workshop* t nil))

(defun ws-descriptor ()
  (workshop-descriptor *workshop*))

(defun ws-projects ()
  (bt:with-lock-held (*workshop-lock*)
    (workshop-projects *workshop*)))

(defun ws-sessions ()
  (bt:with-lock-held (*workshop-lock*)
    (workshop-sessions *workshop*)))

(defun ws-create-project (pdir-rel &rest ctor-kwargs &key &allow-other-keys)
  (bt:with-lock-held (*workshop-lock*)
    (let* ((puuid (apply #'mopr-org:workshop-create-project
                         (mopr-uri:make-desc-chain (workshop-descriptor *workshop*))
                         (workshop-information *workshop*)
                         pdir-rel ctor-kwargs))
           (pcons-unbound (mopr-uri:desc-alist-assoc
                           (mopr-org:workshop-info-projects
                            (workshop-information *workshop*))
                           :uuid puuid))
           (wchain (mopr-uri:make-desc-chain (workshop-descriptor *workshop*)))
           (pcons (ingest-project wchain pcons-unbound)))
      (push pcons (workshop-projects *workshop*)))))

(defun ws-acquire-project (lookup-type lookup-val session-id)
  (bt:with-lock-held (*workshop-lock*)
    (workshop-acquire-project *workshop* lookup-type lookup-val session-id)))

(defun ws-release-project (lookup-type lookup-val session-id)
  (bt:with-lock-held (*workshop-lock*)
    (workshop-release-project *workshop* lookup-type lookup-val session-id)))
