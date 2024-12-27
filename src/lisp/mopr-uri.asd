;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-uri
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description
  "My personal framework for my own OpenUSD related experiments,
mainly in Lisp. UNTESTED. DO NOT USE!"

  :depends-on
  (#:frugal-uuid
   #:quri)

  :pathname "uri"

  :components
  ((:file "package")
   (:file "descriptor"
    :depends-on ("package"))))