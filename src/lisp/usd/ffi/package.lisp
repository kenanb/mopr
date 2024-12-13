;;;; package.lisp

(push :mopr-ffi *features*)

(defpackage #:mopr-ffi
  (:use)
  (:export #:init-core
           #:init-wrap))

(defpackage #:mopr-def
  (:use))

(defpackage #:mopr-fun
  (:use))
