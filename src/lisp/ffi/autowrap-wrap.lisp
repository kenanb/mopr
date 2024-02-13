(cl:in-package :mopr-def)

(autowrap:c-include
 '(#:mopr #:ffi "moprWrapIncludes.h")
 :spec-path '(#:mopr #:ffi #:spec)

 ;; According to current directory structure,
 ;; the header lookup should happen relative
 ;; to the parent directory of system definition.
 :sysincludes
 (cl:list (cl:namestring
           (uiop/pathname:pathname-parent-directory-pathname
            (asdf:component-pathname (asdf:find-system :mopr cl:t)))))

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
 :function-package #:MOPR-FUN
 :symbol-regex
 (("^mopr_(.*)" () (cl:lambda (s m r)
                     (cl:declare (cl:ignorable s r))
                     (cl:elt m 0))))

 :no-accessors cl:t

 :trace-c2ffi cl:t

 ;; Don't mute logs.
 :release-p cl:nil)
