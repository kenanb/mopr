;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-prim)

(defconstant +prim-type-list+
  (list 'mopr-ns:|PointInstancer|
        'mopr-ns:|Camera|
        'mopr-ns:|Xform|
        'mopr-ns:|Mesh|
        'mopr-ns:|NurbsPatch|
        'mopr-ns:|BasisCurves|
        'mopr-ns:|NurbsCurves|
        'mopr-ns:|Points|
        'mopr-ns:|Capsule|
        'mopr-ns:|Cone|
        'mopr-ns:|Cube|
        'mopr-ns:|Cylinder|
        'mopr-ns:|Sphere|))

(defun create-generic-prim-type-tokens (table)
  (loop for s in +prim-type-list+
        for s-upcase = (alexandria:format-symbol "MOPR-NS" "~:@(~A~)" s)
        for s-name = (symbol-name s)
        for val = (mopr:create-token)
        do (progn
             (mopr:token-ctor-cstr val s-name)
             (setf (gethash s table) val)
             (setf (gethash s-upcase table) val))))

(defun delete-generic-prim-type-tokens (table)
  (loop for s in +prim-type-list+
        for val = (gethash s table)
        do (progn
             (mopr:delete-token val)
             (autowrap:invalidate val))))
