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

;; (ql:quickload :alexandria)

;; (ql:quickload :cl-autowrap)

;; (ql:quickload :float-features)

;; (ql:quickload :bordeaux-threads)

;; (ql:quickload :split-sequence)

;; (ql:quickload :frugal-uuid)

;; (ql:quickload :quri)

;; NOTE : The testing implementation of XMLS calls ASDF:SYSTEM-RELATIVE-PATHNAME while defining
;;        *TEST-FILES*. This works fine for the normal CL build environment, but seems to cause the
;;        executable built using asdf:make-build to fail (MISSING-COMPONENT error) during startup.
;;        So I had to workaround it by commenting out the test code in my local clone of the repo.

;; (ql:quickload :xmls)

;; TODO : There is a recent ECL + IRONCLAD issue, causing ironclad:byte-array-to-hex-string
;;        to produce incorrect results: https://github.com/sharplispers/ironclad/issues/78
;;
;;        I observed expected behaviour when I tested with local build that includes commit:
;;        https://github.com/sharplispers/ironclad/commit/f6519450b47a7648f837126e9f269857033e352a

;; (ql:quickload :ironclad/digest/sha1)

(asdf:oos 'asdf:load-op :cffi)
(asdf:oos 'asdf:load-op :alexandria)
(asdf:oos 'asdf:load-op :cl-autowrap)
(asdf:oos 'asdf:load-op :float-features)
(asdf:oos 'asdf:load-op :ironclad/digest/sha1)
(asdf:oos 'asdf:load-op :bordeaux-threads)
(asdf:oos 'asdf:load-op :split-sequence)
(asdf:oos 'asdf:load-op :frugal-uuid)
(asdf:oos 'asdf:load-op :quri)
(asdf:oos 'asdf:load-op :xmls)

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
