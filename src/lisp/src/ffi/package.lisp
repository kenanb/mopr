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

(defpackage #:yoga-ffi
  (:use)
  (:export #:init-yoga))

(defpackage #:yoga-def
  (:use))

(defpackage #:yoga-fun
  (:use))
