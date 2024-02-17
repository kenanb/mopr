;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

;; TODO [2024-02-17] : Move this logic behind user-driven plugin registration.

;; Call table generation.

(defun create-prim-call-table (table)

  ;; Grid generation functions.
  (setf (gethash :grid-extent table) #'prim-fn-grid-extent)
  (setf (gethash :grid-fv-counts table) #'prim-fn-grid-fv-counts)
  (setf (gethash :grid-fv-indices table) #'prim-fn-grid-fv-indices)
  (setf (gethash :grid-points table) #'prim-fn-grid-points)

  (setf (gethash :test-gen-xform-info table) #'prim-fn-test-gen-xform-info))

(defun create-data-call-table (table)
  (setf (gethash :test-gen-cubes table) #'data-fn-test-gen-cubes)
  (setf (gethash :test-tree-gen table) #'data-fn-test-tree-gen))
