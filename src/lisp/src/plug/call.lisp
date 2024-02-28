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

(defun process-data-call-stack (form)
  (process-call-stack form *data-call-table*))

(defun process-prim-call-stack (form)
  (process-call-stack form *prim-call-table*))
