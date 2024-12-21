;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-usd/val)

(defconstant +value-role-list+
  '(;; texCoord2x
    (:|texCoord2d| . :|double2|)
    (:|texCoord2f| . :|float2|)
    (:|texCoord2h| . :|half2|)

    ;; texCoord3x
    (:|texCoord3d| . :|double3|)
    (:|texCoord3f| . :|float3|)
    (:|texCoord3h| . :|half3|)

    ;; point3x
    (:|point3d| . :|double3|)
    (:|point3f| . :|float3|)
    (:|point3h| . :|half3|)

    ;; vector3x
    (:|vector3d| . :|double3|)
    (:|vector3f| . :|float3|)
    (:|vector3h| . :|half3|)

    ;; normal3x
    (:|normal3d| . :|double3|)
    (:|normal3f| . :|float3|)
    (:|normal3h| . :|half3|)

    ;; color3x
    (:|color3d| . :|double3|)
    (:|color3f| . :|float3|)
    (:|color3h| . :|half3|)

    ;; color4x
    (:|color4d| . :|double4|)
    (:|color4f| . :|float4|)
    (:|color4h| . :|half4|)

    ;; misc
    (:|frame4d| . :|matrix4d|)

    (:|group| . :|opaque|)))

(defun get-real-type (type-key)
  (or (cdr (assoc type-key +value-role-list+)) type-key))
