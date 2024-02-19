;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

(defvar *attr-info-xform-op-order*
  (make-instance 'mopr-prop:attr-info
                 :base-name "xformOpOrder"
                 :meta '(:interp :uniform)
                 :array-p t
                 :type-key :token))

(defvar *attr-info-translate*
  (make-instance 'mopr-prop:attr-info
                 :namespace '("xformOp")
                 :base-name "translate"
                 :type-key :float3))

(defvar *attr-info-rotate-x-y-z*
  (make-instance 'mopr-prop:attr-info
                 :namespace '("xformOp")
                 :base-name "rotateXYZ"
                 :type-key :double3))

;; Test functions.

(defun prim-fn-test-gen-xform-info (&rest args)
  (destructuring-bind (tr-array rt-array) args
    (let ((compound (mopr-prop:make-compound)))
      (setf (mopr-prop:compound-properties compound)
            (list
             (mopr-prop:make-property
              :info *attr-info-xform-op-order*
              :data (list #1A (("xformOp" "translate")
                               ("xformOp" "rotateXYZ"))))
             (mopr-prop:make-property
              :info *attr-info-translate*
              :data (list tr-array))
             (mopr-prop:make-property
              :info *attr-info-rotate-x-y-z*
              :data (list rt-array))))
      compound)))

(defun data-fn-test-gen-cubes (&rest args)
  (flet ((define-cube (x r prim-name)
           (let* ((tr (list (mod x r) (floor (/ x r)) 0))
                  (rt (list 0 x x))
                  (tr-a (make-array 3 :initial-contents tr))
                  (rt-a (make-array 3 :initial-contents rt)))
             `(:prim (,prim-name)
                     (:type mopr-ns:Cube)
                     (:attr "size" :datum :double #0A .5)
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
