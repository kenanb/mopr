(cl:in-package :mopr-viz/repr-ffi)

(cffi:define-foreign-library libmopr-repr
  (:unix (:or "libmopr_repr.so"))
  (cl:t (:default "libmopr_repr")))

(cl:defun init-repr ()
  (cffi:use-foreign-library mopr-viz/repr-ffi::libmopr-repr))
