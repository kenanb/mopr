;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-exe
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description
  "My personal framework for my own OpenUSD related experiments,
mainly in Lisp. UNTESTED. DO NOT USE!"

  :depends-on
  (#:mopr-sgt
   #:mopr-usd)

  :pathname "exe"

  :components
  ((:file "package")
   (:file "execute"
    :depends-on ("package"))
   (:file "procedure"
    :depends-on ("package" "execute"))))
