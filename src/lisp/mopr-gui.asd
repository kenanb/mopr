;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
;;

(defsystem #:mopr-gui
  :version "0.0.1"
  :author "Kenan Bölükbaşı"
  :license "BSD-3-Clause"

  :components
  ((:module #:yoga-ffi
    :pathname "gui/yoga-ffi"
    :components
    ((:file "package")
     (:file "autowrap-yoga"
      :depends-on ("package"))
     (:module #:spec
      :depends-on ("autowrap-yoga"))
     (:file "bindings-yoga"
      :depends-on ("package"))
     (:static-file "moprYogaIncludes.h")))

   (:module #:repr-ffi
    :pathname "gui/repr-ffi"
    :depends-on ("yoga-ffi")
    :components
    ((:file "package")
     (:file "autowrap-repr"
      :depends-on ("package"))
     (:module #:spec
      :depends-on ("autowrap-repr"))
     (:file "bindings-repr"
      :depends-on ("package"))
     (:static-file "moprReprIncludes.h"))))

  :depends-on
  (#:mopr))
