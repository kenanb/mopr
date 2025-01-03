;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-usd
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :description
  "My personal framework for my own OpenUSD related experiments,
mainly in Lisp. UNTESTED. DO NOT USE!"

  :depends-on
  (#:uiop
   #:cffi
   #:alexandria
   #:cl-autowrap)

  :pathname "usd"

  :components
  ((:module #:ffi
    :components
    ((:file "package")
     (:file "autowrap-core"
      :depends-on ("package"))
     (:file "autowrap-wrap"
      :depends-on ("package"))
     (:module #:spec
      :depends-on ("autowrap-core"
                   "autowrap-wrap"))
     (:file "bindings-core"
      :depends-on ("package"))
     (:file "bindings-wrap"
      :depends-on ("package"))
     (:static-file "moprWrapIncludes.h")
     (:static-file "moprCoreIncludes.h")))

   (:module #:main
    :depends-on ("ffi")
    :components
    ((:file "package")
     (:file "raii"
      :depends-on ("package"))
     (:file "describe"
      :depends-on ("package" "raii"))))

   (:module #:val
    :depends-on ("main")
    :components
    ((:file "package")
     (:file "types"
      :depends-on ("package"))
     (:file "roles"
      :depends-on ("package" "types"))
     (:file "transfer"
      :depends-on ("package" "types"))))

   (:module #:info
    :depends-on ("main" "val")
    :components
    ((:file "package")
     (:file "shared"
      :depends-on ("package"))
     (:file "prop"
      :depends-on ("package"))
     (:file "value"
      :depends-on ("package"))
     (:file "schema"
      :depends-on ("package" "shared" "prop"))
     (:file "bundle"
      :depends-on ("package" "shared" "value" "schema"))
     (:file "registry"
      :depends-on ("package" "value" "schema" "bundle"))
     (:file "util"
      :depends-on ("package" "prop" "value" "schema" "bundle" "registry"))))))

(register-system-packages "mopr-usd"
                          '(:mopr-usd/ffi
                            :mopr-usd/info
                            :mopr-usd/val))
