;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/util
  (:import-from :mopr)
  (:import-from :mopr-sgt)
  (:import-from :mopr-gui/repr-def)
  (:import-from :mopr-gui/repr-rnode)
  (:import-from :mopr-gui/repr)
  (:use #:cl)
  (:export
   #:populate-from-lisp-file))

(in-package :mopr-ext/util)

(defun test-mopr ()
  (let ((core-ret (mopr:test-core))
        (wrap-std-ret (mopr:test-wrap-std))
        (wrap-usd-ret (mopr:test-wrap-usd)))
    (format t "CORE LIBRARY CALL RETURN VALUE: ~A~%" core-ret)
    (format t "WRAP-STD LIBRARY CALL RETURN VALUE: ~A~%" wrap-std-ret)
    (format t "WRAP-USD LIBRARY CALL RETURN VALUE: ~A~%" wrap-usd-ret)))

(defun get-mopr-user-package ()
  (or (find-package "MOPR-USER")
      (error "Cannot find MOPR-USER package.~%")))

(defun populate-from-lisp-file (layer-h filepath call-enabled)
  "CAUTION: Calls to functions registered to call tables can be dangerous, if enabled."
  (let* ((cn (mopr-sgt:read-from-usds-file filepath (get-mopr-user-package))))
    (mopr-sgt:populate-layer layer-h cn call-enabled)
    cn))
