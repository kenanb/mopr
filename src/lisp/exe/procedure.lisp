;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-exe)

(defun procedure-execute (pr layer-h call-enabled)
  (unless (zerop (mopr-usd:layer-try-upgrade layer-h))
    (mopr-usd:with-handle (stage-h :stage)
      (mopr-usd:stage-open-layer stage-h layer-h)
      ;; (procedure-debug-print pr)
      (let* ((pr-expanded (make-expanded-dnode-procedure pr call-enabled)))
        ;; (procedure-debug-print pr)
        (with-bound-procedure-accessors ((root procedure-root)) pr-expanded
          (execute-all root stage-h))))))

(defun procedure-export-to-usda-string
    (pr call-enabled)
  (mopr-usd:with-handles* ((exported-str :string)
                           (layer-h :layer))
    (mopr-usd:layer-create-anonymous layer-h "")
    (procedure-execute pr layer-h call-enabled)
    (mopr-usd:layer-export-to-string exported-str layer-h)
    (mopr-usd:string-cstr exported-str)))

;; Using NATIVE-NAMESTRING to resolve possible HOME directory references via
;; TILDE character. Even though Lisp code will handle the pathname correctly,
;; foreign library code we eventually pass the NAMESTRING to might not.

(defun procedure-export-to-usd-file
    (pr filepath call-enabled
     &aux (native-filepath-ns (native-namestring filepath)))
  (mopr-utl:validate-simple-path native-filepath-ns)
  (mopr-usd:with-handles* ((layer-h :layer))
    (mopr-usd:layer-create-new layer-h native-filepath-ns)
    (procedure-execute pr layer-h call-enabled)
    (mopr-usd:layer-save layer-h)))
