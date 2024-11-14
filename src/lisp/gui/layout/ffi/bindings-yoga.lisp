(cl:in-package :mopr-gui/yoga-ffi)

(cffi:define-foreign-library libyoga-core
  (:unix (:or "libyoga_core.so"))
  (cl:t (:default "libyoga_core")))

(cl:defun init-yoga ()
  (cffi:use-foreign-library mopr-gui/yoga-ffi::libyoga-core))
