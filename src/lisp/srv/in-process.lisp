;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-srv)

(defun in-process-backend-init (wdir-abs)
  (format t "MOPR initializing in-process backend.~%")
  (format t "Acquiring workshop lock at path: ~A~%" wdir-abs)
  (mopr-msg:acquire-ws wdir-abs))

(defun in-process-backend-term ()
  (format t "MOPR terminating in-process backend.~%")
  (format t "Releasing workshop lock.~%")
  (mopr-msg:release-ws))
