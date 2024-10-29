;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/util
  (:import-from :mopr)
  (:import-from :mopr-ext/enode-execute)
  (:use #:cl
        ;; TODO: UI crashes when this is just an "import-from", instead of "use".
        #:mopr-ext/repr)
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
        (let* ((expr (read in nil))
               (rn (mopr-ext/enode-serialize:deserialize expr nil)))
          (mopr-ext/enode-execute:populate-layer layer-h rn call-enabled))))))

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

        ;; Representation.
        ;;

        (let* ((expr (read in nil)))
          (mopr-ext/repr:create-enode-tree-with-repr expr))

        (mopr-ext/repr:populate-command-queue (autowrap:wrap-pointer cmd-queue
                                                                     'mopr-def:command-queue))
        (mopr-ext/repr:deinitialize-rnodes)       ; TODO : Defer.

        ;; Execution.
        ;;

        (mopr-ext/enode-execute:populate-layer layer-h
                                               mopr-ext/repr::*root-enode*
                                               call-enabled)))))
