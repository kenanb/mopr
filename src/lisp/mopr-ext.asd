;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-ext
  :class asdf:package-inferred-system
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description "This system is used only as a root anchor
for package-inferred extension systems in ext directory."
  :pathname "ext"
  :depends-on (:mopr))
