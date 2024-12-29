(cl:in-package :mopr-viz/yoga-def)

(autowrap:c-include
 '(#:mopr-viz #:layout #:ffi "moprYogaIncludes.h")
 :spec-path '(#:mopr-viz #:layout #:ffi #:spec)

 ;; Only needed during spec generation. So once we generate the spec file,
 ;; we shouldn't need the envvar being defined for later REPL use.
 :sysincludes
 (cl:list (uiop/os:getenv "MOPR_YOGA_INC_DIR"))

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

 :exclude-sources ("^/usr/include/.*")

 :exclude-definitions ("^_[_A-Z].*" ; Skip reserved symbols.
                       "^ARCH_.*")

 ;; Cleanup prefixes of functions, isolate into a separate package.
 :function-package #:MOPR-VIZ/YOGA-FUN

 :symbol-regex
 (("^YG(.*)" () (cl:lambda (s m r)
                     (cl:declare (cl:ignorable s r))
                     (cl:elt m 0))))

 :no-accessors cl:t

 :trace-c2ffi cl:t

 ;; Don't mute logs.
 :release-p cl:nil)

(cl:setf mopr-viz/yoga-def:+undefined+ float-features:single-float-nan)
