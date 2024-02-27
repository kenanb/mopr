;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-plug
  :defsystem-depends-on (#:mopr)
  :class "MOPR-PLUG:CONFIGURATION"
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description "See mopr system."
  :pathname "plug"
  :depends-on
  (:mopr

   ;; ENABLED PLUGIN LIST:
   ;;
   ;; Begin
   ;;

   "mopr-plug/usds"
   "mopr-plug/util"

   ;;
   ;; End
   ;;

   ))
