(cl:in-package :mopr-viz/yoga-ffi)

(cffi:define-foreign-library libyoga-core
  (:unix (:or "libyoga_core.so"))
  (cl:t (:default "libyoga_core")))

(cl:defun init-yoga ()
  (cffi:use-foreign-library mopr-viz/yoga-ffi::libyoga-core))
