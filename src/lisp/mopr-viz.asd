;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-viz
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"

  :depends-on
  (#:mopr-sgt
   #:uiop
   #:cffi
   #:float-features
   #:cl-autowrap)

  :pathname "viz"

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
     (:file "rdata"
      :depends-on ("ffi"))
     (:file "control")
     (:file "rnode"
      :depends-on ("rdata" "ffi"))
     (:file "repr"
      :depends-on ("rdata" "rnode" "ffi" "control"))))))

(register-system-packages "mopr-viz"
                          '(:mopr-viz/repr-def
                            :mopr-viz/repr-ffi
                            :mopr-viz/repr-fun
                            :mopr-viz/repr-shared
                            :mopr-viz/repr-testing
                            :mopr-viz/repr-rdata
                            :mopr-viz/repr-rnode
                            :mopr-viz/repr))

(register-system-packages "mopr-viz"
                          '(:mopr-msg/ctrl))
