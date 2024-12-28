;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-res
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description
  "My personal framework for my own OpenUSD related experiments,
mainly in Lisp. UNTESTED. DO NOT USE!"

  :depends-on
  (#:mopr-uri
   #:uiop)

  :pathname "res"

  :components
  ((:file "package")
   (:file "entity"
    :depends-on ("package"))
   (:file "pndescriptor"
    :depends-on ("package"))
   (:file "pndesc-alist"
    :depends-on ("package" "pndescriptor"))
   (:file "pndesc-chain"
    :depends-on ("package" "pndescriptor"))
   (:file "workshop"
    :depends-on ("package"
                 "entity"
                 "pndescriptor"
                 "pndesc-alist"
                 "pndesc-chain"))))
