;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

(defun process-call-stack (form table)
  (alexandria:when-let ((c (gethash (car form) table)))
    (apply (callable-fn c) (cdr form))))

;; TODO [2024-02-17] : Move this logic behind user-driven plugin registration.

(defstruct (callable (:constructor make-callable (fn in out)))
  (fn nil :read-only t)
  (in nil :read-only t)
  (out nil :read-only t))

;; Call table generation.

(defun create-prim-call-table (table)

  ;; Grid generation functions.

  (setf (gethash :grid-extent table)
        (make-callable #'prim-fn-grid-extent
                       '(:s :x :y :z)
                       '(:prop-entry)))

  (setf (gethash :grid-fv-counts table)
        (make-callable #'prim-fn-grid-fv-counts
                       '(:w :h)
                       '(:prop-entry)))

  (setf (gethash :grid-fv-indices table)
        (make-callable #'prim-fn-grid-fv-indices
                       '(:w :h :dir)
                       '(:prop-entry)))

  (setf (gethash :grid-points table)
        (make-callable #'prim-fn-grid-points
                       '(:s :dims :order)
                       '(:point-based)))

  ;; Test functions.

  (setf (gethash :test-gen-xform-info table)
        (make-callable #'prim-fn-test-gen-xform-info
                       '(:tr-array :rt-array)
                       '(:data-group))))

(defun create-data-call-table (table)

  ;; Test functions.

  (setf (gethash :test-gen-cubes table)
        (make-callable #'data-fn-test-gen-cubes
                       '(:r)
                       '(:data-group)))

  (setf (gethash :test-tree-gen table)
        (make-callable #'data-fn-test-tree-gen
                       '()
                       '(:tree-entry))))
