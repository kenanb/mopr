;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:usds-cn)

(defconstant +prim-type-list+
  (list 'usds-ns:|PointInstancer|
        'usds-ns:|Camera|
        'usds-ns:|Xform|
        'usds-ns:|Mesh|
        'usds-ns:|NurbsPatch|
        'usds-ns:|BasisCurves|
        'usds-ns:|NurbsCurves|
        'usds-ns:|Points|
        'usds-ns:|Capsule|
        'usds-ns:|Cone|
        'usds-ns:|Cube|
        'usds-ns:|Cylinder|
        'usds-ns:|Sphere|))

(defvar *prim-type-table* nil)

(defun create-generic-prim-type-tokens (table)
  (loop for s in +prim-type-list+
        for s-upcase = (alexandria:format-symbol "USDS-NS" "~:@(~A~)" s)
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
