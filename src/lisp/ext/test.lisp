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
                          :o (:group-enode mopr-sgt:group-enode))

    ;; :test-gen-cubes
    ;; #S(mopr-plug:callable :fn data-fn-test-gen-cubes
    ;;                       :i (:r fixnum)
    ;;                       :o (:group-enode mopr-sgt:group-enode))

    :test-tree-gen
    #S(mopr-plug:callable :fn data-fn-test-tree-gen
                          :i ()
                          :o (:tree-enode mopr-sgt:tree-enode))))

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
    (make-instance 'mopr-sgt:prim-schema-prop-enode
                   :info-param *attr-info-xform-op-order*
                   :body-form-param (list #1A (("xformOp" "translate")
                                               ("xformOp" "rotateXYZ"))))
    (make-instance 'mopr-sgt:prim-schema-prop-enode
                   :info-param *attr-info-translate*
                   :body-form-param (list tr-array))
    (make-instance 'mopr-sgt:prim-schema-prop-enode
                   :info-param *attr-info-rotate-x-y-z*
                   :body-form-param (list rt-array)))))

;; TODO : Reimplement support for generating prim entries
;;        and recursive expansion.

;; (defun data-fn-test-gen-cubes (r)
;;   (flet ((define-cube (x r prim-name)
;;            (let* ((tr (list (mod x r) (floor (/ x r)) 0))
;;                   (rt (list 0 x x))
;;                   (tr-a (make-array 3 :initial-contents tr))
;;                   (rt-a (make-array 3 :initial-contents rt)))
;;              (mopr-sgt:make-prim-entry
;;               :data `((,prim-name)
;;                       (:type :Cube)
;;                       (:attr "size" :datum :double #0A .5)
;;                       (:call () ,tr-a ,rt-a :test-gen-xform-info))))))

;;     (loop for x below (* r r)
;;           for prim-name = (format nil "Prim_~4,'0d" x)
;;           collecting (list prim-name :spec :def) into tree
;;           collecting (define-cube x r prim-name) into prims
;;           finally (return (mopr-sgt:make-data-group
;;                            :data (cons
;;                                   (mopr-sgt:make-tree-entry
;;                                    :data tree)
;;                                   prims))))))

(defun data-fn-test-tree-gen ()
  (make-instance
   'mopr-sgt:tree-enode
   :body-form-param
   '(("a" :spec :class)
     ("b"
      ("d"
       ("e" :spec :over :alias :x)
       ("f" :alias :y)))
     ("c"))))
