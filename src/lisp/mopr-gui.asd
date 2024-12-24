;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-gui
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"

  :depends-on
  (#:mopr-sgt
   #:uiop
   #:cffi
   #:float-features
   #:cl-autowrap
   #:cl-plus-c)

  :pathname "gui"

  :components
  ((:module #:layout
    :components
    ((:module #:ffi
      :components
      ((:file "package")
       (:file "autowrap-yoga"
        :depends-on ("package"))
       (:module #:spec
        :depends-on ("autowrap-yoga"))
       (:file "bindings-yoga"
        :depends-on ("package"))
       (:static-file "moprYogaIncludes.h")))
     (:file "shared" :depends-on ("ffi"))
     (:file "testing"
      :depends-on ("ffi" "shared"))))

   (:module #:repr
    :depends-on ("layout")
    :components
    ((:module #:ffi
      :components
      ((:file "package")
       (:file "autowrap-repr"
        :depends-on ("package"))
       (:module #:spec
        :depends-on ("autowrap-repr"))
       (:file "bindings-repr"
        :depends-on ("package"))
       (:static-file "moprReprIncludes.h")))
     (:file "identifier")
     (:file "shared")
     (:file "rdata"
      :depends-on ("shared" "ffi"))
     (:file "rnode"
      :depends-on ("shared" "rdata" "ffi"))
     (:file "repr"
      :depends-on ("identifier" "shared" "rdata" "rnode" "ffi"))))))

(register-system-packages "mopr-gui"
                          '(:mopr-gui/identifier
                            :mopr-gui/repr-def
                            :mopr-gui/repr-ffi
                            :mopr-gui/repr-fun
                            :mopr-gui/repr-shared
                            :mopr-gui/repr-testing
                            :mopr-gui/repr-rdata
                            :mopr-gui/repr-rnode
                            :mopr-gui/repr))
