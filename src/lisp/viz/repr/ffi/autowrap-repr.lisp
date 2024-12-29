(cl:in-package :mopr-viz/repr-def)

(autowrap:c-include
 '(#:mopr-viz #:repr #:ffi "moprReprIncludes.h")
 :spec-path '(#:mopr-viz #:repr #:ffi #:spec)

 ;; According to current directory structure,
 ;; the header lookup should happen relative
 ;; to the parent directory of system definition.
 :sysincludes
 (cl:list (cl:namestring
           (uiop/pathname:pathname-parent-directory-pathname
            (uiop/pathname:pathname-parent-directory-pathname
             (asdf:component-pathname (asdf:find-system :mopr-viz cl:t))))))

 ;; For now, we limit spec generation to avoid committing too many changes.
 :exclude-arch
 ("i686-pc-linux-gnu"
  ;; "x86_64-pc-linux-gnu"
  "i686-pc-windows-msvc"
  "x86_64-pc-windows-msvc"
  "i686-apple-darwin9"
  "x86_64-apple-darwin9"
  "i386-unknown-freebsd"
  "x86_64-unknown-freebsd"
  "i386-unknown-openbsd"
  "x86_64-unknown-openbsd"
  "arm-pc-linux-gnu"
  "aarch64-pc-linux-gnu"
  "arm-unknown-linux-androideabi"
  "aarch64-unknown-linux-android"
  "powerpc64-pc-linux-gnu"
  "powerpc64le-pc-linux-gnu"
  "i686-unknown-linux-android"
  "x86_64-unknown-linux-android")

 :exclude-definitions ("^_[_A-Z].*" ; Skip reserved symbols.
                       "^ARCH_.*")

 ;; Cleanup prefixes of functions, isolate into a separate package.
 :function-package #:MOPR-VIZ/REPR-FUN
 :symbol-regex
 (("^mopr_(.*)" () (cl:lambda (s m r)
                     (cl:declare (cl:ignorable s r))
                     (cl:elt m 0))))

 ;; Access using cl-plus-c: There seems to be some issues with
 ;; autowrap accessors approach:
 ;; - combined-command union is failing on things like c-aref calls
 ;;   unless we define a type alias around it.
 ;; - nested union > struct > array-member > index based access decides
 ;;   array type is a pointer instead of char.
 ;; The remaining accessors work as expected.

 :no-accessors cl:t

 :trace-c2ffi cl:t

 ;; Don't mute logs.
 :release-p cl:nil)
