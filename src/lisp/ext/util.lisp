;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/util
  (:import-from :mopr)
  (:import-from :mopr-sgt)
  (:import-from :mopr-gui/repr-def)
  (:import-from :mopr-gui/repr-rnode)
  (:import-from :mopr-gui/repr)
  (:use #:cl
        ;; TODO: UI crashes when this is just an "import-from", instead of "use".
        #:mopr-gui/repr)
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
               (rn (mopr-sgt:deserialize expr nil)))
          (mopr-sgt:populate-layer layer-h rn call-enabled))))))

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

        (let* ((expr (read in nil))
               (rn (mopr-sgt:deserialize expr '(mopr-gui/repr-rnode:rnode))))
          (mopr-sgt:populate-layer layer-h rn call-enabled)

          ;; Representation.
          ;;

          (mopr-gui/repr:initialize-and-bind-repr-tree rn)

          (mopr-gui/repr:populate-command-queue
           (autowrap:wrap-pointer cmd-queue 'mopr-gui/repr-def:command-queue))

          ;; TODO : Defer.
          (mopr-gui/repr:deinitialize-rnodes))))))
