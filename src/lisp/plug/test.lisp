;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

(defvar *attr-info-xform-op-order*
  (make-instance 'mopr-sgt:attr-info
                 :base-name "xformOpOrder"
                 :meta '(:interp :uniform)
                 :array-p t
                 :type-key :token))

(defvar *attr-info-translate*
  (make-instance 'mopr-sgt:attr-info
                 :namespace '("xformOp")
                 :base-name "translate"
                 :type-key :float3))

(defvar *attr-info-rotate-x-y-z*
  (make-instance 'mopr-sgt:attr-info
                 :namespace '("xformOp")
                 :base-name "rotateXYZ"
                 :type-key :double3))

;; Test functions.

(defun prim-fn-test-gen-xform-info (tr-array rt-array)
  (let ((data-group (mopr-sgt:make-data-group)))
    (setf (mopr-sgt:data-group-data data-group)
          (list
           (mopr-sgt:make-prop-entry
            :info *attr-info-xform-op-order*
            :data (list #1A (("xformOp" "translate")
                             ("xformOp" "rotateXYZ"))))
           (mopr-sgt:make-prop-entry
            :info *attr-info-translate*
            :data (list tr-array))
           (mopr-sgt:make-prop-entry
            :info *attr-info-rotate-x-y-z*
            :data (list rt-array))))
    data-group))

(defun data-fn-test-gen-cubes (r)
  (flet ((define-cube (x r prim-name)
           (let* ((tr (list (mod x r) (floor (/ x r)) 0))
                  (rt (list 0 x x))
                  (tr-a (make-array 3 :initial-contents tr))
                  (rt-a (make-array 3 :initial-contents rt)))
             (mopr-sgt:make-prim-entry
              :data `((,prim-name)
                      (:type mopr-ns:Cube)
                      (:attr "size" :datum :double #0A .5)
                      (:call ,tr-a ,rt-a :test-gen-xform-info))))))

    (loop for x below (* r r)
          for prim-name = (format nil "Prim_~4,'0d" x)
          collecting (list prim-name :spec :def) into tree
          collecting (define-cube x r prim-name) into prims
          finally (return (mopr-sgt:make-data-group
                           :data (cons
                                  (mopr-sgt:make-tree-entry
                                   :data tree)
                                  prims))))))

(defun data-fn-test-tree-gen ()
  (mopr-sgt:make-tree-entry
   :data '(("a" :spec :class)
           ("b"
            ("d"
             ("e" :spec :over :alias :x)
             ("f" :alias :y)))
           ("c"))))
