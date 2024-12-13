;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-exe)

(defun procedure-execute (pr layer-h call-enabled)
  (unless (zerop (mopr-usd:layer-try-upgrade layer-h))
    (mopr-usd:with-handle (stage-h :stage)
      (mopr-usd:stage-open-layer stage-h layer-h)
      ;; (procedure-debug-print pr)
      (let* ((pr-preprocessed (make-preprocessed-dnode-procedure pr call-enabled)))
        ;; (procedure-debug-print pr)
        (procedure-call pr-preprocessed #'execute-all stage-h)))))
