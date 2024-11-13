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
  (:export))

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
  (let ((rn (mopr-sgt:read-from-usds-file filepath (get-mopr-user-package))))
    (mopr-sgt:populate-layer layer-h rn call-enabled)))

(defun populate-from-lisp-file-with-repr (layer-h cmd-queue filepath call-enabled)
  "CAUTION: Calls to functions registered to call tables can be dangerous, if enabled."
  (let ((rn (mopr-sgt:read-from-usds-file filepath (get-mopr-user-package)
                                          '(mopr-gui/repr-rnode:rnode))))
    (mopr-sgt:populate-layer layer-h rn call-enabled)

    (mopr-gui/repr:initialize-and-bind-repr-tree rn)
    (mopr-gui/repr:populate-command-queue
     (autowrap:wrap-pointer cmd-queue 'mopr-gui/repr-def:command-queue))
    ;; TODO : Defer.
    (mopr-gui/repr:deinitialize-rnodes)))
