;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr
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
   #:cl-autowrap
   #:float-features)

  :components
  ((:module #:ffi
    :pathname "src/ffi"
    :components
    ((:file "package")
     (:file "autowrap-core"
      :depends-on ("package"))
     (:file "autowrap-wrap"
      :depends-on ("package"))
     (:file "autowrap-yoga"
      :depends-on ("package"))
     (:module #:spec
      :depends-on ("autowrap-core"
                   "autowrap-wrap"
                   "autowrap-yoga"))
     (:file "bindings-core"
      :depends-on ("package"))
     (:file "bindings-wrap"
      :depends-on ("package"))
     (:file "bindings-yoga"
      :depends-on ("package"))
     (:static-file "moprYogaIncludes.h")
     (:static-file "moprWrapIncludes.h")
     (:static-file "moprCoreIncludes.h")))

   (:module #:mopr
    :pathname "src/mopr"
    :depends-on ("ffi")
    :components
    ((:file "package")
     (:file "raii"
      :depends-on ("package"))
     (:file "describe"
      :depends-on ("package" "raii"))))

   (:module #:val
    :pathname "src/val"
    :depends-on ("mopr")
    :components
    ((:file "package")
     (:file "types"
      :depends-on ("package"))
     (:file "roles"
      :depends-on ("package" "types"))
     (:file "transfer"
      :depends-on ("package" "types"))))

   (:module #:info
    :pathname "src/info"
    :depends-on ("mopr" "val")
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
      :depends-on ("package" "prop" "value" "schema" "bundle" "registry"))))

   (:module #:plug
    :pathname "src/plug"
    :depends-on ("mopr" "info")
    :components
    ((:file "package")
     (:file "config"
      :depends-on ("package"))
     (:file "call"
      :depends-on ("package" "config"))
     (:file "generic"
      :depends-on ("package" "config" "call"))))

   (:module #:sgt
    :pathname "src/sgt"
    :depends-on ("mopr" "info" "plug")
    :components
    ((:file "package")
     (:file "enode"
      :depends-on ("package"))
     (:file "sgt"
      :depends-on ("package" "enode"))))))

(register-system-packages "mopr"
                          '(:mopr-ffi
                            :mopr-info
                            :mopr-plug
                            :mopr-sgt
                            :mopr-val))
