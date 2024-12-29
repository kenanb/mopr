;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-org)

(defstruct entity-info
  "ENTITY-INFO

Base class of workshop content.
"
  (created-time (get-universal-time)
   :type integer
   :read-only t)
  (description ""
   :type string))

(defstruct (asset-info (:include entity-info))
  "ASSET-INFO

An asset is mainly an abstraction over a file. Assets can be procedures, scene
description and other media.
"
  (content-type :source
   :type keyword))

(defstruct (project-info (:include entity-info))
  "PROJECT-INFO

A project is mainly an abstraction over a filesystem directory that is a
container of assets.

At any time, a single client connected to the server (the server currently
assumed to be holding the lock to the workshop) is assumed to be working on a
specific project. This is expected to be tracked by the WORKSHOP instance.
"
  (assets nil
   :type list))
