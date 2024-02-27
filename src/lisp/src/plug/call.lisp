;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct callable
    (fn nil :read-only t)
    (i nil :read-only t)
    (o nil :read-only t))

  (defmethod make-load-form ((s callable) &optional environment)
    (declare (ignore environment))
    (make-load-form-saving-slots s)))

(defun process-call-stack (form table)
  (loop with stack = nil
        for e in form
        for c = (gethash e table)
        if c
          do (let (args)
               (loop for i below (length (callable-i c))
                     do (push (pop stack) args))
               (push (apply (callable-fn c) args) stack))
        else
          do (push e stack)
        end
        finally (return (reverse stack))))

;; TODO [2024-02-17] : Move this logic behind user-driven plugin registration.

;; Call table generation.

(defconstant +call-table-prim+
  '(;; Grid generation functions.
    :grid-extent
    #S(mopr-plug:callable :fn mopr-plug::prim-fn-grid-extent
                          :i (:size :x :y :z)
                          :o (:prop-entry))

    :grid-fv-counts
    #S(mopr-plug:callable :fn mopr-plug::prim-fn-grid-fv-counts
                          :i (:w :h)
                          :o (:prop-entry))

    :grid-fv-indices
    #S(mopr-plug:callable :fn mopr-plug::prim-fn-grid-fv-indices
                          :i (:w :h :dir)
                          :o (:prop-entry))

    :grid-points
    #S(mopr-plug:callable :fn mopr-plug::prim-fn-grid-points
                          :i (:size :dims :order)
                          :o (:point-based))

    ;; Test functions.
    :test-grid-oscillate
    #S(mopr-plug:callable :fn mopr-plug::prim-fn-grid-oscillate
                          :i (:pbg :length :dim)
                          :o (:point-based))

    :test-gen-xform-info
    #S(mopr-plug:callable :fn mopr-plug::prim-fn-test-gen-xform-info
                          :i (:tr-array :rt-array)
                          :o (:data-group))))

(defconstant +call-table-data+
  '(;; Test functions.
    :test-gen-cubes
    #S(mopr-plug:callable :fn mopr-plug::data-fn-test-gen-cubes
                          :i (:r)
                          :o (:data-group))

    :test-tree-gen
    #S(mopr-plug:callable :fn mopr-plug::data-fn-test-tree-gen
                          :i ()
                          :o (:tree-entry))))

(defun create-prim-call-table (table)
  (loop for (k v . rest) on +call-table-prim+ by #'cddr
        do (setf (gethash k table) v)))

(defun create-data-call-table (table)
  (loop for (k v . rest) on +call-table-data+ by #'cddr
        do (setf (gethash k table) v)))
