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
   #:mopr-exe
   #:mopr-uri
   #:mopr-viz
   #:bordeaux-threads
   #:xmls)

  :pathname "msg"

  :components
  ((:file "package")
   (:file "handler-base"
    :depends-on ("package"))
   (:file "session"
    :depends-on ("package"))
   (:file "workshop"
    :depends-on ("package"))
   (:file "direct-calls"
    :depends-on ("package"))
   (:module #:handler
    :depends-on ("package" "handler-base" "session" "workshop" "direct-calls")
    :components
    ((:file "absolute")
     (:file "relative")
     (:file "workshop")
     (:file "project")
     (:file "asset")
     (:file "working")))
   (:file "dispatch"
    :depends-on ("package"))
   (:file "handler-main"
    :depends-on ("package" "dispatch" "handler-base"))))
