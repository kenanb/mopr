;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-uri)

(defstruct (desc-chain
            (:constructor make-desc-chain (&rest contents)))
  "DESC-CHAIN

DESC-CHAIN is a list of DESCRIPTORs, the ordering of which is VERY IMPORTANT,
because it usually denotes the location of a resource in a hierarchical
structure. In other words, it is used to build various kinds of resource paths,
e.g URI paths.
"
  (contents nil :type list))

(defun desc-chain-as-uuid (chain)
  "DESC-CHAIN-AS-UUID

Joins the uuids of each descriptor with given delimiter.

EXAMPLE: (A B C) => uuid(A) + delimiter + uuid(B) + delimiter + uuid(C)
"
  (format nil "~{~A~^/~}" (mapcar #'descriptor-uuid (desc-chain-contents chain))))
