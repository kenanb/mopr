;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defstruct entity-info
  "ENTITY-INFO

Base class of workshop content.
"
  (created-time (get-universal-time)
   :type integer
   :read-only t)
  (description ""
   :type string))

(defstruct (resource-info (:include entity-info))
  "RESOURCE-INFO

A resource is mainly an abstraction over a file. Resources can be procedures,
scene description and other media.
"
  (content-type :source
   :type keyword))

(defstruct (project-info (:include entity-info))
  "PROJECT-INFO

A project is mainly an abstraction over a filesystem directory that is a
container of resources.

At any time, a single client connected to the server (the server currently
assumed to be holding the lock to the workshop) is assumed to be working on a
specific project. This is expected to be tracked by the WORKSHOP instance.
"
  (resources nil
   :type list))
