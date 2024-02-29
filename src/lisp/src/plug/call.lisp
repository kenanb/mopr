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

;; During evaluation:
;;
;; STACK :
;;
;; PUSH  : a
;;
;; STACK : a << CAR
;;
;; PUSH  : b
;; PUSH  : c
;;
;; STACK : a
;;         b
;;         c << CAR
;;
;; CALL  : f (m n -- p q)
;;            : :
;;            b c
;;
;; STACK : a
;;         p
;;         q << CAR
;;
;; End of evaluation:
;; We return the stack reversed:
;;
;; RETURN: '(a p q)

(defun process-call-stack (form special-table generic-table)
  (loop with stack = nil
        for e in form
        for c = (or (gethash e special-table)
                    (gethash e generic-table))
        if c do (let (args)
                  (loop for i below (length (callable-i c))
                        do (push (pop stack) args))
                  (dolist (x (multiple-value-list
                              (apply (callable-fn c) args)))
                    (push x stack)))
          else do (push e stack)
        end
        finally (return (reverse stack))))

(defun process-data-call-stack (form)
  (process-call-stack form *data-call-table* *generic-call-table*))

(defun process-prim-call-stack (form)
  (process-call-stack form *prim-call-table* *generic-call-table*))
