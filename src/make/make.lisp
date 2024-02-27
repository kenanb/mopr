;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(require :asdf)

(push #P"./src/lisp/" asdf:*central-registry*)

;; ; Some debug variables:
;;
;; (setf compiler::*debug-compiler* t)
;; or
;; (setf compiler::*delete-files* nil)

;; If there is an existing QL installation, that can be used instead
;; by means of manually adding QL init code here, or removing "-norc" option
;; from ecl instantiation in the makefile.

;; (require :ecl-quicklisp)

;; (ql:quickload :cffi)

;; (ql:quickload :cl-autowrap)

(asdf:oos 'asdf:load-op :cffi)
(asdf:oos 'asdf:load-op :cl-autowrap)

(asdf:oos 'asdf:load-op :mopr)
(asdf:oos 'asdf:load-op :mopr-plug)

(asdf:make-build :mopr-user
                 :type :shared-library
                 :monolithic t
                 :move-here "./"
                 ;; Needed for the system to locate the uiop dependency of cffi.
                 :prologue-code '(require :asdf)
                 ;; Alternative, to avoid requiring ASDF.
                 ;; :prologue-code '(progn (defpackage :uiop/os)
                 ;;                  (defpackage :uiop/pathname)
                 ;;                  (defpackage :uiop/filesystem)
                 ;;                  (defpackage :asdf/operate)
                 ;;                  (defpackage :asdf/lisp-action))
                 :init-name "mopr_init_lisp_module_all")


;; ; If compiler is in debug mode, the files to be deleted will be postponed till exit.
;;
;; (format t "compiler::*files-to-be-deleted*:~%~S~%" compiler::*files-to-be-deleted*)

(quit)
