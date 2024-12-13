;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-val)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-ctor-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR-USD" "~A-~A-CTOR" value-kind x))
  (defun get-row-major-aref-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR-USD" "~A-~A-ROW-MAJOR-AREF" value-kind x))
  (defun get-value-assign-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR-USD" "VALUE-ASSIGN-~A-~A" value-kind x))

  ;; Array only:
  (defun get-resize-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR-USD" "~A-~A-RESIZE" value-kind x))
  (defun get-reserve-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR-USD" "~A-~A-RESERVE" value-kind x))
  (defun get-aref-fn-symbol (value-kind x)
    (alexandria:format-symbol "MOPR-USD" "~A-~A-AREF" value-kind x)))

(defmacro %get-transfer-for-type-fn (category elt-type cffi-type type-string)
  (let ((datum-h-sym (gensym (format nil "~A-~A-H-G" category type-string)))
        (value-array-sym (gensym "VALUE-ARRAY-G"))
        (value-h-sym (gensym "VALUE-H-G")))
    `(lambda ,(list value-array-sym value-h-sym)
       (mopr-usd:with-handles* ((,datum-h-sym ,(format nil "~A-~A" category type-string)))
         (,(get-ctor-fn-symbol category type-string) ,datum-h-sym)
         ,(when (eq category :array)
            ;; For now, we just resize and accept value-initialization cost.
            ;; It seems value-init ctor of Gf types don't initialize members anyway.
            ;; (mopr-usd:array-float3-reserve ,datum-h-sym (array-dimension ,value-array-sym 0))
            `(,(get-resize-fn-symbol category type-string)
              ,datum-h-sym
              (array-dimension ,value-array-sym 0)))
         (loop for i below (array-total-size ,value-array-sym)
               for elt = (,(get-row-major-aref-fn-symbol category type-string) ,datum-h-sym i)
               ;; do (format t "WRITING for ~A: ~S ~%" i (row-major-aref ,value-array-sym i))
               ;; NOTE: CFFI API is more suitable here.
               ;; Autowrap style access (autowrap:c-aref) does extra stuff we don't need,
               ;; and it doesn't support all primitive types, like ":bool".
               do (setf (cffi:mem-ref elt ,cffi-type)
                        (coerce (row-major-aref ,value-array-sym i) ',elt-type)))
         (,(get-value-assign-fn-symbol category type-string) ,value-h-sym ,datum-h-sym)))))

(defun token-transfer-datum-handler (value-array value-h)
  (mopr-usd:with-handles* ((datum-h "DATUM-TOKEN"))
    (#.(get-ctor-fn-symbol :datum :token) datum-h)
    (loop for i below (array-total-size value-array)
          for elt = (#.(get-row-major-aref-fn-symbol :datum :token) datum-h i)
          for val = (row-major-aref value-array i)
          for val-str = (etypecase val
                          (list (format nil "~{~A~^:~}" val))
                          (symbol (symbol-name val))
                          (string val))
          ;; do (format t "WRITING for ~A: ~S ~%" i (row-major-aref value-array i))
          do (mopr-usd:token-ctor-cstr elt val-str))
    (#.(get-value-assign-fn-symbol :datum :token) value-h datum-h)))

(defun token-transfer-array-handler (value-array value-h)
  (mopr-usd:with-handles* ((datum-h "ARRAY-TOKEN"))
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
          do (mopr-usd:token-ctor-cstr elt val-str))
    (#.(get-value-assign-fn-symbol :array :token) value-h datum-h)))

(defmacro %transfer-for-type (category real-type)
  `(case ,real-type
     ,@(loop for (real-type elt-type cffi-type format-type) in +value-type-list+
             for type-string = (format nil "~:@(~A~)" (or format-type real-type))
             when cffi-type
               collect (list real-type
                             (list '%get-transfer-for-type-fn
                                   category elt-type cffi-type type-string)))
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
