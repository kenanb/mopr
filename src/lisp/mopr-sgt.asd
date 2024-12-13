;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-sgt
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description
  "My personal framework for my own OpenUSD related experiments,
mainly in Lisp. UNTESTED. DO NOT USE!"

  :depends-on
  (#:mopr ;; TODO : Move out the execute module and remove dependency.
   #:ironclad/digest/sha1)

  :pathname "sgt"

  :components
  ((:module #:plug
    :components
    ((:file "package")
     (:file "config"
      :depends-on ("package"))
     (:file "call"
      :depends-on ("package" "config"))
     (:file "generic"
      :depends-on ("package" "config" "call"))))
   (:file "package")
   (:module #:payload
    :depends-on ("package")
    :components
    ((:file "base")
     (:file "container" :depends-on ("base"))
     (:file "directive" :depends-on ("base"))
     (:file "statement" :depends-on ("base"))))
   (:file "header"
    :depends-on ("package" "payload"))
   (:file "bnode"
    :depends-on ("package" "payload"))
   (:file "cnode"
    :depends-on ("package" "payload" "header" "bnode"))
   (:file "dnode"
    :depends-on ("package" "payload" "bnode"))
   (:file "node-serialize"
    :depends-on ("package" "payload" "header" "cnode"))
   (:file "node-preprocess"
    :depends-on ("package" "payload" "header" "cnode" "dnode" "plug"))
   (:file "node-callables"
    :depends-on ("package" "payload" "header" "cnode" "dnode" "plug"))
   (:file "node-execute"
    :depends-on ("package" "payload" "header" "cnode"))
   (:file "enode"
    :depends-on ("package" "payload" "header" "cnode"))
   (:file "procedure"
    :depends-on
    ("package"
     "payload"
     "header"
     "bnode"
     "cnode"
     "node-serialize"
     "node-preprocess"
     "node-execute"
     "enode"))))

(register-system-packages "mopr-sgt"
                          '(:mopr-plug))