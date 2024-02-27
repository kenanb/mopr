;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

;; NOTE: If module  A depends-on a module B that  happens to call
;; use-foreign-library, the  call triggers the library  search at
;; compile time, which fails due to lookup paths.  So we use USER
;; system to  not only define  the idiomatic "user"  package, but
;; also to  ensure initialization happening after  all dependency
;; systems are loaded.

(defsystem #:mopr-user
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :depends-on
  (#:mopr
   #:mopr-plug)
  :components
  ((:file "mopr-user")))
