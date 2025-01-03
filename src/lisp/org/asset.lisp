;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-org)

(defstruct (asset-info (:include entity-info))
  "ASSET-INFO

An asset is mainly an abstraction over a file. Assets can be procedures, scene
description and other media.
"
  (content-type :source
   :type keyword))
