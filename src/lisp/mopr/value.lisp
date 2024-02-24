;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-val)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +value-type-list+
    (list
     ;; misc
     '(:|bool| ((unsigned-byte 1)) :bool)
     '(:|uchar| ((unsigned-byte 8)) :unsigned-char)
     '(:|uint| ((unsigned-byte 32)) :unsigned-int)
     '(:|int64| ((signed-byte 64)) :int64)
     '(:|uint64| ((unsigned-byte 64)) :uint64)

     '(:|timecode| (double-float) :double)
     '(:|string| (string))
     '(:|token| (keyword))
     '(:|asset| ((or pathname string)))
     '(:|opaque| (null)) ; Only scalar allowed.
     '(:|pathExpression| (string))

     ;; int
     '(:|int| ((signed-byte 32)) :int)
     '(:|int2| ((signed-byte 32) (2)) :int)
     '(:|int3| ((signed-byte 32) (3)) :int)
     '(:|int4| ((signed-byte 32) (4)) :int)

     ;; double
     '(:|double| (double-float) :double)
     '(:|double2| (double-float (2)) :double)
     '(:|double3| (double-float (3)) :double)
     '(:|double4| (double-float (4)) :double)

     ;; float
     '(:|float| (single-float) :float)
     '(:|float2| (single-float (2)) :float)
     '(:|float3| (single-float (3)) :float)
     '(:|float4| (single-float (4)) :float)

     ;; half
     '(:|half| (short-float))
     '(:|half2| (short-float (2)))
     '(:|half3| (short-float (3)))
     '(:|half4| (short-float (4)))

     ;; quat
     '(:|quatd| (double-float (4)) :double)
     '(:|quatf| (single-float (4)) :float)
     '(:|quath| (short-float (4)))

     ;; matrix
     '(:|matrix2d| (double-float (2 2)) :double)
     '(:|matrix3d| (double-float (3 3)) :double)
     '(:|matrix4d| (double-float (4 4)) :double)))

  "Value type list data format:
  - real-type
  - (elt-type &optional dims)
  - &optional primitive-type format-type")

(defconstant +value-role-list+
  (list

   ;; texCoord2x
   '(:|texCoord2d| . :|double2|)
   '(:|texCoord2f| . :|float2|)
   '(:|texCoord2h| . :|half2|)

   ;; texCoord3x
   '(:|texCoord3d| . :|double3|)
   '(:|texCoord3f| . :|float3|)
   '(:|texCoord3h| . :|half3|)

   ;; point3x
   '(:|point3d| . :|double3|)
   '(:|point3f| . :|float3|)
   '(:|point3h| . :|half3|)

   ;; vector3x
   '(:|vector3d| . :|double3|)
   '(:|vector3f| . :|float3|)
   '(:|vector3h| . :|half3|)

   ;; normal3x
   '(:|normal3d| . :|double3|)
   '(:|normal3f| . :|float3|)
   '(:|normal3h| . :|half3|)

   ;; color3x
   '(:|color3d| . :|double3|)
   '(:|color3f| . :|float3|)
   '(:|color3h| . :|half3|)

   ;; color4x
   '(:|color4d| . :|double4|)
   '(:|color4f| . :|float4|)
   '(:|color4h| . :|half4|)

   ;; misc
   '(:|frame4d| . :|matrix4d|)
   '(:|group| . :|opaque|)))

(defun get-real-type (type-key)
  (or (cdr (assoc type-key +value-role-list+)) type-key))

(defstruct (value-type (:type vector) :named
                       (:constructor make-value-type
                           (real-type
                            elt-type
                            &optional
                              (dims nil)
                            &aux
                              (scalar-type-name (mopr:create-value-type-name))
                              (vector-type-name (mopr:create-value-type-name))
                              (rank (length dims))
                              (nof-elt (apply #'* dims)))))
  "The VALUE-TYPE struct consisting  of :REAL-TYPE :ELT-TYPE :SCALAR-TYPE-NAME :VECTOR-TYPE-NAME :DIMS :RANK and :NOF-ELT information."
  (real-type
   (error "VALUE-TYPE should have a REAL-TYPE.")
   :type (or list symbol class)
   :read-only t)
  (elt-type
   (error "VALUE-TYPE should have an ELT-TYPE.")
   :type (or list symbol class)
   :read-only t)
  (scalar-type-name
   (error "VALUE-TYPE should have a SCALAR-TYPE-NAME.")
   :type t
   :read-only t)
  (vector-type-name
   (error "VALUE-TYPE should have a VECTOR-TYPE-NAME.")
   :type t
   :read-only t)
  (dims
   nil
   :type list
   :read-only t)
  (rank
   nil
   :type (unsigned-byte 7)
   :read-only t)
  (nof-elt
   1
   :type (unsigned-byte 7)
   :read-only t))

(defun value-type-name (value-type value-type-array-p)
  (if value-type-array-p
      (value-type-vector-type-name value-type)
      (value-type-scalar-type-name value-type)))

(defun get-complete-type-list ()
  (mapcar #'car (append +value-type-list+ +value-role-list+)))

(defun create-generic-value-type-table (table)
  (loop for s in (get-complete-type-list)
        for s-upcase = (alexandria:format-symbol "KEYWORD" "~:@(~A~)" s)
        for real-type = (get-real-type s)
        for type-info = (assoc real-type +value-type-list+)
        for val = (apply #'make-value-type (car type-info) (cadr type-info))
        for s-name = (symbol-name s)
        ;; Value: ( [scalar form] . [vector form] )
        do (progn
             (mopr:value-type-name-find-cstr (value-type-scalar-type-name val)
                                             s-name)
             (mopr:value-type-name-find-cstr (value-type-vector-type-name val)
                                             (format nil "~A[]" s-name))
             (setf (gethash s table) val)
             (setf (gethash s-upcase table) val))))

(defun delete-generic-value-type-table (table)
  (loop for s in (get-complete-type-list)
        for val = (gethash s table)
        for val-scalar = (value-type-scalar-type-name val)
        for val-vector = (value-type-vector-type-name val)
        do (progn
             (mopr:delete-value-type-name val-scalar)
             (autowrap:invalidate val-scalar)
             (mopr:delete-value-type-name val-vector)
             (autowrap:invalidate val-vector))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-ctor-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR" "~A-~A-CTOR" value-kind x))
  (defun get-row-major-aref-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR" "~A-~A-ROW-MAJOR-AREF" value-kind x))
  (defun get-value-assign-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR" "VALUE-ASSIGN-~A-~A" value-kind x))

  ;; Array only:
  (defun get-resize-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR" "~A-~A-RESIZE" value-kind x))
  (defun get-reserve-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR" "~A-~A-RESERVE" value-kind x))
  (defun get-aref-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR" "~A-~A-AREF" value-kind x)))

(defmacro %get-transfer-for-type-fn (category elt-type primitive-type type-string)
  (let ((datum-h-sym (gensym (format nil "~A-~A-H-G" category type-string)))
        (value-array-sym (gensym "VALUE-ARRAY-G"))
        (value-h-sym (gensym "VALUE-H-G")))
    `(lambda ,(list value-array-sym value-h-sym)
       (mopr:with-handles* ((,datum-h-sym ,(format nil "~A-~A" category type-string)))
         (,(get-ctor-fn-symbol category type-string) ,datum-h-sym)
         ,(when (eq category :array)
            ;; For now, we just resize and accept value-initialization cost.
            ;; It seems value-init ctor of Gf types don't initialize members anyway.
            ;; (mopr:array-float3-reserve ,datum-h-sym (array-dimension ,value-array-sym 0))
            `(,(get-resize-fn-symbol category type-string)
              ,datum-h-sym
              (array-dimension ,value-array-sym 0)))
         (loop for i below (array-total-size ,value-array-sym)
               for elt = (,(get-row-major-aref-fn-symbol category type-string) ,datum-h-sym i)
               ;; do (format t "WRITING for ~A: ~S ~%" i (row-major-aref ,value-array-sym i))
               ;; NOTE: CFFI API is more suitable here.
               ;; Autowrap style access (autowrap:c-aref) does extra stuff we don't need,
               ;; and it doesn't support all primitive types, like ":bool".
               do (setf (cffi:mem-ref elt ,primitive-type)
                        (coerce (row-major-aref ,value-array-sym i) ',elt-type)))
         (,(get-value-assign-fn-symbol category type-string) ,value-h-sym ,datum-h-sym)))))

(defun token-transfer-datum-handler (value-array value-h)
  (mopr:with-handles* ((datum-h "DATUM-TOKEN"))
    (#.(get-ctor-fn-symbol :datum :token) datum-h)
    (loop for i below (array-total-size value-array)
          for elt = (#.(get-row-major-aref-fn-symbol :datum :token) datum-h i)
          for val = (row-major-aref value-array i)
          for val-str = (etypecase val
                          (list (format nil "~{~A~^:~}" val))
                          (symbol (symbol-name val))
                          (string val))
          ;; do (format t "WRITING for ~A: ~S ~%" i (row-major-aref value-array i))
          do (mopr:token-ctor-cstr elt val-str))
    (#.(get-value-assign-fn-symbol :datum :token) value-h datum-h)))

(defun token-transfer-array-handler (value-array value-h)
  (mopr:with-handles* ((datum-h "ARRAY-TOKEN"))
    (#.(get-ctor-fn-symbol :array :token) datum-h)
    (#.(get-resize-fn-symbol :array :token) datum-h
       (array-dimension value-array 0))
    (loop for i below (array-total-size value-array)
          for elt = (#.(get-row-major-aref-fn-symbol :array :token) datum-h i)
          for val = (row-major-aref value-array i)
          for val-str = (etypecase val
                          (list (format nil "~{~A~^:~}" val))
                          (symbol (symbol-name val))
                          (string val))
          ;; do (format t "WRITING for ~A: ~S ~%" i (row-major-aref value-array i))
          do (mopr:token-ctor-cstr elt val-str))
    (#.(get-value-assign-fn-symbol :array :token) value-h datum-h)))

(defmacro %transfer-for-type (category real-type)
  `(case ,real-type
     ,@(loop for (real-type (elt-type dims) primitive-type format-type) in +value-type-list+
             for type-string = (format nil "~:@(~A~)" (or format-type real-type))
             when primitive-type
               collect (list real-type
                             (list '%get-transfer-for-type-fn
                                   category elt-type primitive-type type-string)))
     (:|token|
      #',(alexandria:symbolicate "TOKEN-TRANSFER-" category "-HANDLER"))
     (otherwise nil)))

(defun get-transfer-for-type-function (real-type datum-array-p)
  (if datum-array-p
      (%transfer-for-type :array real-type)
      (%transfer-for-type :datum real-type)))

(defun transfer-for-type (real-type datum-array-p value-array value-h)
  (let ((transfer-for-type-fn
          (get-transfer-for-type-function real-type datum-array-p)))
    (if transfer-for-type-fn
        (prog1 t
          (funcall transfer-for-type-fn value-array value-h))
        nil)))
