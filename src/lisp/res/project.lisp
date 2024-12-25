;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defstruct project
  "PROJECT

A project represents a combination of procedures, scene description and other
media.

At any time, a single client connected to the server (the server currently
assumed to be holding the lock to the workshop) is assumed to be working on a
specific project. This is expected to be tracked by the WORKSHOP instance.

LOCATION: Workshop-relative path of the project directory.
"
  (location (error "PROJECT cannot be initialized without a location!")
   :type pathname
   :read-only t))
