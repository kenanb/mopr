;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-ops
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description
  "My personal framework for my own OpenUSD related experiments,
mainly in Lisp. UNTESTED. DO NOT USE!"

  :depends-on
  (#:mopr-sgt)

  :pathname "ops"

  :components
  ((:file "package")
   (:file "node-id"
    :depends-on ("package"))
   (:file "collect"
    :depends-on ("package" "node-id"))
   (:file "options"
    :depends-on ("package"))))
