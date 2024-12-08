;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/test
  (:import-from :mopr)
  (:use #:cl #:mopr-ext/grid)
  (:export))

(in-package :mopr-ext/test)

;; Call table generation.

(defconstant +callables+
  '(:test-gen-xform-info
    #S(mopr-plug:callable :fn prim-fn-test-gen-xform-info
                          :i (:tr-array array :rt-array array)
                          :o (:group-dnode mopr-sgt:dnode))

    :test-gen-cubes
    #S(mopr-plug:callable :fn data-fn-test-gen-cubes
                          :i (:r fixnum)
                          :o (:group-dnode mopr-sgt:dnode))

    :test-tree-gen
    #S(mopr-plug:callable :fn data-fn-test-tree-gen
                          :i ()
                          :o (:tree-dnode mopr-sgt:dnode))))

(defvar *attr-info-xform-op-order*
  (make-instance 'mopr-info:attr-info
                 :base-name "xformOpOrder"
                 :meta '(:interp :uniform)
                 :array-p t
                 :type-key :token))

(defvar *attr-info-translate*
  (make-instance 'mopr-info:attr-info
                 :namespace '("xformOp")
                 :base-name "translate"
                 :type-key :float3))

(defvar *attr-info-rotate-x-y-z*
  (make-instance 'mopr-info:attr-info
                 :namespace '("xformOp")
                 :base-name "rotateXYZ"
                 :type-key :double3))

;; Test functions.

(defun prim-fn-test-gen-xform-info (tr-array rt-array)
  (mopr-sgt:make-group
   (list
    (mopr-sgt:make-dnode
     :payload
     (mopr-sgt:make-prim-schema-prop-statement
      :info-param *attr-info-xform-op-order*
      :body-form-param (list #1A (("xformOp" "translate")
                                  ("xformOp" "rotateXYZ")))))
    (mopr-sgt:make-dnode
     :payload
     (mopr-sgt:make-prim-schema-prop-statement
      :info-param *attr-info-translate*
      :body-form-param (list tr-array)))
    (mopr-sgt:make-dnode
     :payload
     (mopr-sgt:make-prim-schema-prop-statement
      :info-param *attr-info-rotate-x-y-z*
      :body-form-param (list rt-array))))))

(defun data-fn-test-gen-cubes (r)
  (flet ((define-cube (x r prim-name)
           (let* ((tr (list (mod x r) (floor (/ x r)) 0))
                  (rt (list 0 x x))
                  (tr-a (make-array 3 :initial-contents tr))
                  (rt-a (make-array 3 :initial-contents rt))
                  (prim-node
                    (mopr-sgt:make-dnode
                     :payload
                     (mopr-sgt:make-prim-statement
                      :path-form-param (list prim-name))))
                  (prim-type-payload
                    (mopr-sgt:make-prim-type-statement
                     :name-param :Cube))
                  (size-attr-payload
                    (mopr-sgt:make-prim-attr-statement
                     :name-param (coerce "size" 'base-string)
                     :category-param :datum
                     :type-param :double
                     :body-form-param (list #0A .5)))
                  (call-payload
                    (mopr-sgt:make-prim-call-directive
                     :aux-form-param nil
                     :body-form-param (list tr-a rt-a :test-gen-xform-info)))
                  (prim-node-children (list prim-type-payload size-attr-payload call-payload)))
             (loop for p in prim-node-children
                   do (vector-push-extend (mopr-sgt:make-dnode :payload p)
                                          (mopr-sgt:dnode-children prim-node)))
             prim-node)))

    (loop for x below (* r r)
          for prim-name = (format nil "Prim_~4,'0d" x)
          collecting (list prim-name :spec :def) into tree
          collecting (define-cube x r prim-name) into prims
          finally (return (mopr-sgt:make-group
                           (cons (mopr-sgt:make-dnode :payload
                                                      (mopr-sgt:make-tree-statement
                                                       :body-form-param tree))
                                 prims))))))

(defun data-fn-test-tree-gen ()
  (mopr-sgt:make-dnode
   :payload
   (mopr-sgt:make-tree-statement
    :body-form-param
    '(("a" :spec :class)
      ("b"
       ("d"
        ("e" :spec :over :alias :x)
        ("f" :alias :y)))
      ("c")))))
