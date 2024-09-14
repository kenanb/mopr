;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/util
  (:import-from :mopr)
  (:use #:cl
        #:mopr-ext/repr
        #:mopr-ext/usds)
  (:export))

(in-package :mopr-ext/util)

(defun test-mopr ()
  (let ((core-ret (mopr:test-core))
        (wrap-std-ret (mopr:test-wrap-std))
        (wrap-usd-ret (mopr:test-wrap-usd)))
    (format t "CORE LIBRARY CALL RETURN VALUE: ~A~%" core-ret)
    (format t "WRAP-STD LIBRARY CALL RETURN VALUE: ~A~%" wrap-std-ret)
    (format t "WRAP-USD LIBRARY CALL RETURN VALUE: ~A~%" wrap-usd-ret)))

(defun populate-from-lisp-file (layer-h filepath call-enabled
                                &aux
                                  (pkg-mopr-user
                                   (or (find-package "MOPR-USER")
                                       (error "Cannot find MOPR-USER package.~%"))))
  "Only to be used for repository tests! Even though READ-EVAL is disabled,
relying on READ for data is still dangerous! Also, calls to functions
registered to call tables can be dangerous, if enabled."
  (with-open-file (in filepath)
    (with-standard-io-syntax
      (let ((*package* pkg-mopr-user)
            ;; Assignments based on uiop/stream:with-safe-io-syntax .
            (*print-readably* nil)
            (*read-eval* nil))
        (funcall (if call-enabled
                     #'write-to-layer-call-enabled
                     #'write-to-layer)
                 layer-h (read in nil))))))

(defun populate-from-lisp-file-with-repr (layer-h cmd-queue filepath call-enabled
                                          &aux
                                            (pkg-mopr-user
                                             (or (find-package "MOPR-USER")
                                                 (error "Cannot find MOPR-USER package.~%"))))
  "Only to be used for repository tests! Even though READ-EVAL is disabled,
relying on READ for data is still dangerous! Also, calls to functions
registered to call tables can be dangerous, if enabled."
  (with-open-file (in filepath)
    (with-standard-io-syntax
      (let ((*package* pkg-mopr-user)
            ;; Assignments based on uiop/stream:with-safe-io-syntax .
            (*print-readably* nil)
            (*read-eval* nil))
        (let ((expr (read in nil)))
          (populate-command-queue (autowrap:wrap-pointer cmd-queue 'mopr-def:command-queue) expr)
          (funcall (if call-enabled
                       #'write-to-layer-call-enabled
                       #'write-to-layer)
                   layer-h expr))))))
