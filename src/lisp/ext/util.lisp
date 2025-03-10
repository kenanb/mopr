;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/util
  (:import-from :mopr-exe)
  (:import-from :mopr-sgt)
  (:import-from :mopr-usd)
  (:use #:cl)
  (:export
   #:populate-from-lisp-file))

(in-package :mopr-ext/util)

(defun test-mopr ()
  (let ((core-ret (mopr-usd:test-core))
        (wrap-std-ret (mopr-usd:test-wrap-std))
        (wrap-usd-ret (mopr-usd:test-wrap-usd)))
    (format t "CORE LIBRARY CALL RETURN VALUE: ~A~%" core-ret)
    (format t "WRAP-STD LIBRARY CALL RETURN VALUE: ~A~%" wrap-std-ret)
    (format t "WRAP-USD LIBRARY CALL RETURN VALUE: ~A~%" wrap-usd-ret)))

(defun populate-from-lisp-file (layer-h filepath call-enabled)
  "CAUTION: Calls to functions registered to call tables can be dangerous, if enabled."
  (let ((pr (mopr-sgt:make-cnode-procedure-from-usds-file filepath)))
    (mopr-exe:procedure-execute pr layer-h call-enabled)
    pr))
