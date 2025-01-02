;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defvar *messaging-session* nil
  "This special variable will be dynamically bound to a per-client
MESSAGING-SESSION instance during request handler execution.")

(defclass messaging-session ()
  ((id
    :type integer
    :initform 0
    :accessor messaging-session-id
    :documentation "Session ID.")
   (puuid
    :type (or null string)
    :initform nil
    :accessor messaging-session-puuid
    :documentation "UUID of the project currently locked by this session.")
   (assets
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :reader messaging-session-assets
    :documentation "Map of asset UUID to runtime representation of assets.")))
