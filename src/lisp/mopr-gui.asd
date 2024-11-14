;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-gui
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"

  :components
  ((:module #:layout-ffi
    :pathname "gui/layout/ffi"
    :components
    ((:file "package")
     (:file "autowrap-yoga"
      :depends-on ("package"))
     (:module #:spec
      :depends-on ("autowrap-yoga"))
     (:file "bindings-yoga"
      :depends-on ("package"))
     (:static-file "moprYogaIncludes.h")))

   (:module #:layout
    :pathname "gui/layout"
    :depends-on ("layout-ffi")
    :components
    ((:file "shared")
     (:file "testing"
      :depends-on ("shared"))))

   (:module #:repr-ffi
    :pathname "gui/repr/ffi"
    :components
    ((:file "package")
     (:file "autowrap-repr"
      :depends-on ("package"))
     (:module #:spec
      :depends-on ("autowrap-repr"))
     (:file "bindings-repr"
      :depends-on ("package"))
     (:static-file "moprReprIncludes.h")))

   (:module #:repr
    :pathname "gui/repr"
    :depends-on ("repr-ffi" "layout-ffi" "layout")
    :components
    ((:file "shared")
     (:file "rdata"
      :depends-on ("shared"))
     (:file "rnode"
      :depends-on ("shared" "rdata"))
     (:file "repr"
      :depends-on ("shared" "rdata" "rnode")))))

  :depends-on
  (#:mopr
   #:uiop
   #:cffi
   #:alexandria
   #:float-features
   #:cl-autowrap
   #:cl-plus-c))

(register-system-packages "mopr-gui"
                          '(:mopr-gui/repr-def
                            :mopr-gui/repr-ffi
                            :mopr-gui/repr-fun
                            :mopr-gui/repr-shared
                            :mopr-gui/repr-testing
                            :mopr-gui/repr-rdata
                            :mopr-gui/repr-rnode
                            :mopr-gui/repr))
