;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-val)

(defun get-real-type (type-key)
  (or (cdr (assoc type-key +value-role-list+)) type-key))

(defun build-value-type-name (tn &aux (tn-h (mopr:create-value-type-name)))
  (mopr:value-type-name-find-cstr tn-h tn)
  tn-h)

(defun calculate-value-type-dims (scalar-tn-h &aux dims)
  (let ((dim-0 (mopr:value-type-name-get-dimension scalar-tn-h 0))
        (dim-1 (mopr:value-type-name-get-dimension scalar-tn-h 1)))
    (unless (zerop dim-1) (push dim-1 dims))
    (unless (zerop dim-0) (push dim-0 dims))
    dims))

(defstruct (value-type
            (:constructor make-value-type
                (tname
                 &aux
                   (real-type (get-real-type tname))
                   (elt-type (cadr (assoc real-type +value-type-list+)))
                   (scalar-type-name (build-value-type-name (format nil "~A" tname)))
                   (vector-type-name (build-value-type-name (format nil "~A[]" tname)))
                   (dims (calculate-value-type-dims scalar-type-name))
                   (rank (length dims))
                   (nof-elt (apply #'* dims)))))
  "The VALUE-TYPE struct consisting  of :REAL-TYPE :ELT-TYPE :SCALAR-TYPE-NAME
:VECTOR-TYPE-NAME :DIMS :RANK and :NOF-ELT information."
  (real-type
   (error "VALUE-TYPE should have a REAL-TYPE.")
   :type (or list symbol class)
   :read-only t)
  (elt-type
   (error "VALUE-TYPE should have an ELT-TYPE.")
   :type (or list symbol class)
   :read-only t)
  (scalar-type-name
   (error "VALUE-TYPE should have a SCALAR-TYPE-NAME.")
   :type mopr:mopr-value-type-name-h
   :read-only t)
  (vector-type-name
   (error "VALUE-TYPE should have a VECTOR-TYPE-NAME.")
   :type mopr:mopr-value-type-name-h
   :read-only t)
  (dims
   nil
   :type list
   :read-only t)
  (rank
   nil
   :type (unsigned-byte 7)
   :read-only t)
  (nof-elt
   1
   :type (unsigned-byte 7)
   :read-only t))

(defmethod mopr-reg:teardown-entry ((val value-type)
                                  &aux
                                    (val-scalar (value-type-scalar-type-name val))
                                    (val-vector (value-type-vector-type-name val)))
  ;; (format t "DELETING VTYPE : ~A~%" val)
  (mopr:delete-value-type-name val-scalar)
  (autowrap:invalidate val-scalar)
  (mopr:delete-value-type-name val-vector)
  (autowrap:invalidate val-vector))

(defun value-type-name (value-type value-type-array-p)
  (if value-type-array-p
      (value-type-vector-type-name value-type)
      (value-type-scalar-type-name value-type)))
