;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :mopr-plug)

;;;;;;;;;;
;;; Config
;;
;;

(declaim (type (or symbol string) *default-config-system*))

(defvar *default-config-system* '#:mopr-user
  "Configuration system name to be used when no system name is supplied
as an argument to the SYSTEM parameter in a call to CONFIGURE.")

(declaim (type (or null configuration) *config*))

(defvar *config* nil
  "System object that holds configuration data.")

(defvar *repository* nil
  "Directory containing the MOPR package. It is used to load extra data
distributed with the source. Its value is computed from the location
of the MOPR system.")

(defvar *default-resource-root* (uiop/pathname:ensure-directory-pathname "res")
  "The relative directory path default resource hierarchy resides in.")

(defclass configuration (asdf:package-inferred-system)
  ((resource-root
    :initarg :resource-root
    :type pathname-designator
    :initform (uiop/pathname:merge-pathnames* *default-resource-root* *repository*)
    :accessor resource-root
    :documentation "Pathname of the resource root directory to look for sources."))
  (:documentation "System definition class for MOPR configuration."))

(defun configure (&key (system *default-config-system*) force)
  "Loads the configuration system."
  (check-type system (or symbol string))
  (asdf:clear-system system)
  ;; Set the repository before to initialize RESOURCE-ROOT slot of *CONFIG*.
  (setf *repository* (asdf:system-source-directory '#:mopr))
  (if force
      (asdf:load-system system :force force))
  (setf *config* (asdf:find-system system)))
