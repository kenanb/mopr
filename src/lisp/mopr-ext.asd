;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-ext
  :class asdf:package-inferred-system
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description "See mopr system."
  :pathname "ext"
  :depends-on
  (:mopr

   ;; ENABLED PLUGIN LIST:
   ;;
   ;; Begin
   ;;

   "mopr-ext/grid"
   "mopr-ext/test"
   "mopr-ext/usds"
   "mopr-ext/util"

   ;;
   ;; End
   ;;

   ))
