;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/util
  (:import-from :mopr)
  (:use #:cl #:mopr-ext/usds)
  (:export))

(in-package :mopr-ext/util)

(defun test-mopr ()
  (let ((core-ret (mopr:test-core))
        (wrap-std-ret (mopr:test-wrap-std))
        (wrap-usd-ret (mopr:test-wrap-usd)))
    (format t "CORE LIBRARY CALL RETURN VALUE: ~A~%" core-ret)
    (format t "WRAP-STD LIBRARY CALL RETURN VALUE: ~A~%" wrap-std-ret)
    (format t "WRAP-USD LIBRARY CALL RETURN VALUE: ~A~%" wrap-usd-ret)))

;; TODO [2023-02-27]: Consider using uiop/stream:with-safe-io-syntax

(defun populate-from-lisp-file-read-eval-disabled (layer-h filepath call-enabled)
  "Only to be used for repository tests! Even though READ-EVAL is disabled,
relying on READ for data is still dangerous! Also, calls to functions
registered to call tables can be dangerous, if enabled."
  (let ((*read-eval* nil))
    (with-open-file (in filepath)
      (funcall (if call-enabled
                   #'write-to-layer-call-enabled
                   #'write-to-layer)
               layer-h (read in nil)))))
