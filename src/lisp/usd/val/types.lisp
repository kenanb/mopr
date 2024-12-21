;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-usd/val)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +value-type-list+
    '(;; misc
      (:|bool|   (unsigned-byte 1)  :bool)
      (:|uchar|  (unsigned-byte 8)  :unsigned-char)
      (:|uint|   (unsigned-byte 32) :unsigned-int)
      (:|int64|  (signed-byte 64)   :int64)
      (:|uint64| (unsigned-byte 64) :uint64)

      (:|timecode| double-float :double)

      (:|string| string)

      (:|token| keyword)

      (:|asset| (or pathname string))

      (:|opaque| null) ; Only scalar allowed.

      (:|pathExpression| string) ; :path-expression

      ;; int
      (:|int|  (signed-byte 32) :int)
      (:|int2| (signed-byte 32) :int)
      (:|int3| (signed-byte 32) :int)
      (:|int4| (signed-byte 32) :int)

      ;; double
      (:|double|  double-float :double)
      (:|double2| double-float :double)
      (:|double3| double-float :double)
      (:|double4| double-float :double)

      ;; float
      (:|float|  single-float :float)
      (:|float2| single-float :float)
      (:|float3| single-float :float)
      (:|float4| single-float :float)

      ;; half
      (:|half|  short-float)
      (:|half2| short-float)
      (:|half3| short-float)
      (:|half4| short-float)

      ;; quat
      (:|quatd| double-float :double)
      (:|quatf| single-float :float)
      (:|quath| short-float)

      ;; matrix
      (:|matrix2d| double-float :double)
      (:|matrix3d| double-float :double)
      (:|matrix4d| double-float :double))

    "Value type list data format:
- real-type
- elt-type
- &optional cffi-type format-type

If CFFI-TYPE is provided, an auto value transfer handler is registered.
Otherwise, the type is not supported unless a custom handler is registered.
If FORMAT-TYPE is provided, it is used to generate the function names
to call in value transfer handler. Otherwise, the real-type is used."))
