;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

(defstruct (callable (:constructor make-callable (fn in out)))
  (fn nil :read-only t)
  (in nil :read-only t)
  (out nil :read-only t))

(defun process-call-stack (form table)
  (loop with stack = nil
        for e in form
        for c = (gethash e table)
        if c
          do (let (args)
               (loop for i below (length (callable-in c))
                     do (push (pop stack) args))
               (push (apply (callable-fn c) args) stack))
        else
          do (push e stack)
        end
        finally (return (reverse stack))))

;; TODO [2024-02-17] : Move this logic behind user-driven plugin registration.

;; Call table generation.

(defun create-prim-call-table (table)

  ;; Grid generation functions.

  (setf (gethash :grid-extent table)
        (make-callable #'prim-fn-grid-extent
                       '(:size :x :y :z)
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
                       '(:size :dims :order)
                       '(:point-based)))

  ;; Test functions.

  (setf (gethash :test-grid-oscillate table)
        (make-callable #'prim-fn-grid-oscillate
                       '(:pbg :length :dim)
                       '(:point-based)))

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
