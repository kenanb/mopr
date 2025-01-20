;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-org)

(defstruct (pndescriptor
            (:include mopr-uri:descriptor)
            (:constructor)
            (:constructor make-new-pndescriptor
                ;; ECL doesn't associate parameters with included structure slots
                ;; without explicit declaration of matching symbol packages.
                (mopr-uri::role path &aux (mopr-uri::uuid (mopr-uri:new-uuid path)))))
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

;; Using NATIVE-NAMESTRING to resolve possible HOME directory references via
;; TILDE character. Even though Lisp code will handle the pathname correctly,
;; foreign library code we eventually pass the NAMESTRING to might not.

(defun make-pndescriptor-for-file (role file)
  (let* ((file-native (native-namestring file))
         (path (file-pathname-p file-native)))
    (unless path (error "File descriptor requested for non-file path!"))
    (mopr-utl:validate-simple-path path)
    (make-new-pndescriptor role path)))

(defun make-pndescriptor-for-directory (role directory)
  (let* ((directory-native (native-namestring directory))
         (path (ensure-directory-pathname directory-native)))
    (mopr-utl:validate-simple-path path)
    (make-new-pndescriptor role path)))
