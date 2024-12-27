;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-uri)

(defun rchain-descriptor-uuids (descriptors)
  "RCHAIN-DESCRIPTOR-UUIDS

Chains the uuids of each descriptor, in REVERSE order. EXAMPLE:

Merging a list of descriptors (CD BD AD) that would have the uuids ('2' '1' '0'),
correspondingly, will generate '0/1/2'.
"
  (format nil "~{~A~^/~}" (reverse (mapcar #'descriptor-uuid descriptors))))
