(cl:in-package :mopr-ffi)

(cffi:define-foreign-library libmopr-repr
  (:unix (:or "libmopr_repr.so"))
  (cl:t (:default "libmopr_repr")))

(cl:defun init-repr ()
  (cffi:use-foreign-library mopr-ffi::libmopr-repr))
