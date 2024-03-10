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

(defun process-call-stack% (form table)
  (loop with stack = nil
        with params = (car form)
        for e in (cdr form)
        for p = (getf params e :param-not-found)
        if (eq p :param-not-found)
          do (alexandria:if-let ((c (gethash e table)))
               (let (args)
                 (loop for i below (length (callable-i c))
                       do (push (pop stack) args))
                 (dolist (x (multiple-value-list
                             (apply (callable-fn c) args)))
                   (push x stack)))
               (push e stack))
        else
          do (push p stack)
        end
        finally (return (reverse stack))))

(defun process-call-stack (form)
  (process-call-stack% form *call-table*))
