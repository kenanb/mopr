;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-msg
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description
  "My personal framework for my own OpenUSD related experiments,
mainly in Lisp. UNTESTED. DO NOT USE!"

  :depends-on
  (#:mopr-org
   #:mopr-sgt
   #:mopr-uri
   #:bordeaux-threads
   #:xmls)

  :pathname "msg"

  :components
  ((:file "package")
   (:file "control")
   (:file "workshop"
    :depends-on ("package"))))

(register-system-packages "mopr-msg"
                          '(:mopr-msg/ctrl))
