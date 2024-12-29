;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defstruct (pndescriptor
            (:include mopr-uri:descriptor)
            (:constructor)
            (:constructor make-new-pndescriptor
                (role path &aux (uuid (mopr-uri:new-uuid path)))))
  "PATHNAME DESCRIPTOR

A pndescriptor represents the means to unambiguously refer to a resource ( an
asset or asset grouping ) that is directly associated with a pathname.

Every entity that has an associated descriptor will be associated with a UUID at
creation-time. Normally, this will be a UUIDv7, so that the underlying content
can be moved while maintaining stable addressing by the client application. But
it will be possible to configure the UUID version to enable deterministic
addressing. TODO: Implement setting.
"
  (path (error "PNDESCRIPTOR cannot be initialized without a path!")
   :type pathname
   :read-only t))

(defun make-pndescriptor-for-file (role file)
  (make-new-pndescriptor
   role
   (or (file-pathname-p file)
       (error "File descriptor requested for non-file path!"))))

(defun make-pndescriptor-for-directory (role directory)
  (make-new-pndescriptor
   role
   (ensure-directory-pathname directory)))
