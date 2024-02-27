(cl:in-package :mopr-ffi)

(cffi:define-foreign-library libmopr-wrap-std
  (:unix (:or "libmopr_wrap_std.so"))
  (cl:t (:default "libmopr_wrap_std")))

(cffi:define-foreign-library libmopr-wrap-usd
  (:unix (:or "libmopr_wrap_usd.so"))
  (cl:t (:default "libmopr_wrap_usd")))

(cl:defun init-wrap ()
  (cffi:use-foreign-library mopr-ffi::libmopr-wrap-std)
  (cffi:use-foreign-library mopr-ffi::libmopr-wrap-usd))
