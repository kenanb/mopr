(cl:in-package :yoga-ffi)

(cffi:define-foreign-library libyoga-core
  (:unix (:or "libyoga_core.so"))
  (cl:t (:default "libyoga_core")))

(cl:defun init-yoga ()
  (cffi:use-foreign-library yoga-ffi::libyoga-core))
