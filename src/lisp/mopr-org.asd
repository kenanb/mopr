;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-org
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description
  "My personal framework for my own OpenUSD related experiments,
mainly in Lisp. UNTESTED. DO NOT USE!"

  :depends-on
  (#:mopr-uri
   #:mopr-utl
   #:uiop)

  :pathname "org"

  :components
  ((:file "package")
   (:file "entity"
    :depends-on ("package"))
   (:file "asset"
    :depends-on ("package" "entity"))
   (:file "project"
    :depends-on ("package" "entity" "asset"))
   (:file "pndescriptor"
    :depends-on ("package"))
   (:file "pndesc-alist"
    :depends-on ("package" "pndescriptor"))
   (:file "pndesc-chain"
    :depends-on ("package" "pndescriptor"))
   (:file "workshop"
    :depends-on ("package"
                 "entity"
                 "asset"
                 "project"
                 "pndescriptor"
                 "pndesc-alist"
                 "pndesc-chain"))))
