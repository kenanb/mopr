;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem "mopr"
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"
  :depends-on (#:uiop
               #:cffi
               #:alexandria
               #:cl-autowrap)
  :components ((:module #:ffi
                :pathname "ffi"
                :components
                ((:file "package")
                 (:file "autowrap-core" :depends-on ("package"))
                 (:file "autowrap-wrap" :depends-on ("package"))
                 (:module #:spec :depends-on ("autowrap-core"
                                              "autowrap-wrap"))
                 (:file "bindings-core" :depends-on ("package"))
                 (:file "bindings-wrap" :depends-on ("package"))
                 (:static-file "moprWrapIncludes.h")
                 (:static-file "moprCoreIncludes.h")))
               (:module "mopr"
                :depends-on ("ffi")
                :components
                ((:file "package")
                 (:file "raii" :depends-on ("package"))
                 (:file "describe" :depends-on ("package" "raii"))))
               (:module "val"
                :depends-on ("mopr")
                :components
                ((:file "package")
                 (:file "types" :depends-on ("package"))
                 (:file "roles" :depends-on ("package" "types"))
                 (:file "transfer" :depends-on ("package" "types"))))
               (:module "info"
                :depends-on ("mopr")
                :components
                ((:file "package")
                 (:file "util" :depends-on ("package"))
                 (:file "prop" :depends-on ("package"))
                 (:file "value" :depends-on ("package"))
                 (:file "schema" :depends-on ("package" "util" "prop"))
                 (:file "registry" :depends-on ("package" "util" "prop" "value" "schema"))))
               (:module "sgt"
                :depends-on ("mopr" "info")
                :components
                ((:file "package")
                 (:file "sgt" :depends-on ("package"))))
               (:module "plug"
                :depends-on ("mopr" "info" "sgt")
                :components
                ((:file "package")
                 (:file "grid" :depends-on ("package"))
                 (:file "test" :depends-on ("package"))
                 (:file "call" :depends-on ("package" "grid" "test"))
                 (:file "plug" :depends-on ("package" "call"))))
               (:module "usds"
                :depends-on ("mopr" "val" "info" "sgt" "plug")
                :components
                ((:file "package")
                 (:file "usds" :depends-on ("package"))))
               (:module "util"
                :depends-on ("mopr" "usds")
                :components
                ((:file "package")
                 (:file "test" :depends-on ("package"))))
               ;; NOTE: If module A depends-on a module B that happens
               ;; to call use-foreign-library, the call triggers the
               ;; library search at compile time, which fails due to
               ;; lookup paths.
               ;; TODO [2024-01-29] : Investigate the right time for initialization.
               (:module "user"
                :depends-on ("ffi" "mopr" "usds" "util")
                :components
                ((:file "package")
                 (:file "user" :depends-on ("package")))))
  :description "My personal framework for my own OpenUSD related experiments, mainly in Lisp. UNTESTED. DO NOT USE!")
