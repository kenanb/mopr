;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

;; Test functions.

(defun prim-fn-test-gen-xform-info (&rest args)
  (destructuring-bind (tr-array rt-array) args
    `((:prop ("xformOpOrder" :interp :uniform)
       :array :token #1A (("xformOp" "translate")
                          ("xformOp" "rotateXYZ")))
      (:ns "xformOp"
           (:prop "translate" :float3 ,tr-array)
           (:prop "rotateXYZ" :double3 ,rt-array)))))

(defun data-fn-test-gen-cubes (&rest args)
  (flet ((define-cube (x r prim-name)
           (let* ((tr (list (mod x r) (floor (/ x r)) 0))
                  (rt (list 0 x x))
                  (tr-a (make-array 3 :initial-contents tr))
                  (rt-a (make-array 3 :initial-contents rt)))
             `(:prim (,prim-name)
                     (:type mopr-ns:Cube)
                     (:prop "size" :double #0A .5)
                     (:call :test-gen-xform-info ,tr-a ,rt-a)))))
    (destructuring-bind (r) args
      (loop for x below (* r r)
            for prim-name = (format nil "Prim_~4,'0d" x)
            collecting (list prim-name :spec :def) into tree
            collecting (define-cube x r prim-name) into prims
            finally (return `((:tree ,@tree) ,@prims))))))

(defun data-fn-test-tree-gen (&rest args)
  ;; TODO: We don't need args for this.
  (declare (ignore args))
  '((:tree
     ("a" :spec :class)
     ("b"
      ("d"
       ("e" :spec :over :alias :x)
       ("f" :alias :y)))
     ("c"))))
