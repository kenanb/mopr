(cl:in-package :mopr-ffi)

(cffi:define-foreign-library libmopr-core
  (:unix (:or "libmopr_core.so"))
  (cl:t (:default "libmopr_core")))

(cl:defun init-core ()
  (cffi:use-foreign-library mopr-ffi::libmopr-core))
