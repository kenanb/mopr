(cl:in-package :mopr-usd/ffi)

(cffi:define-foreign-library libmopr-core
  (:unix (:or "libmopr_core.so"))
  (cl:t (:default "libmopr_core")))

(cl:defun init-core ()
  (cffi:use-foreign-library mopr-usd/ffi::libmopr-core))
