;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (payload
            (:constructor nil)
            (:copier nil)))

(defun payload-calculate-digest (payload)
  (let* ((payload-str (prin1-to-string payload))
         (octets (ironclad:ascii-string-to-byte-array payload-str))
         (digest (ironclad:digest-sequence :sha1 octets)))
    (ironclad:byte-array-to-hex-string digest)))
