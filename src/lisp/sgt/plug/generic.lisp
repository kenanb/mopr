;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

;; Basic arithmetic operators.
(defun add (x y) (+ x y))
(defun sub (x y) (- x y))
(defun mul (x y) (* x y))
(defun div (x y) (/ x y))
(defun neg (x  ) (- x))
(defun inc (x  ) (1+ x))
(defun dec (x  ) (1- x))

(defconstant +arith-op-callables+
  '(:add #S(callable :fn add :i (:x number :y number) :o (:result number))
    :sub #S(callable :fn sub :i (:x number :y number) :o (:result number))
    :mul #S(callable :fn mul :i (:x number :y number) :o (:result number))
    :div #S(callable :fn div :i (:x number :y number) :o (:result number))
    :neg #S(callable :fn neg :i (:x number          ) :o (:result number))
    :inc #S(callable :fn inc :i (:x number          ) :o (:result number))
    :dec #S(callable :fn dec :i (:x number          ) :o (:result number))))

;; Stack shuffling operators. Based on Factor naming conventions.
(defun dup   (x    ) (values x x))
(defun 2dup  (x y  ) (values x y x y))
(defun over  (x y  ) (values x y x))
(defun 2over (x y z) (values x y z x y))
(defun pick  (x y z) (values x y z x))
(defun dupd  (x y  ) (values x x y))

(defun drop  (x    ) (declare (ignore x  )) (values))
(defun 2drop (x y  ) (declare (ignore x y)) (values))
(defun nip   (x y  ) (declare (ignore x  )) (values y))
(defun 2nip  (x y z) (declare (ignore x y)) (values z))

(defun swap  (x y  ) (values y x))
(defun swapd (x y z) (values y x z))
(defun rot   (x y z) (values y z x))
(defun -rot  (x y z) (values z x y))

(defconstant +stack-op-callables+
  '(:dup   #S(callable :fn dup   :i (:x t)           :o (:x t :x t))
    :2dup  #S(callable :fn 2dup  :i (:x t :y t)      :o (:x t :y t :x t :y t))
    :over  #S(callable :fn over  :i (:x t :y t)      :o (:x t :y t :x t))
    :2over #S(callable :fn 2over :i (:x t :y t :z t) :o (:x t :y t :z t :x t :y t))
    :pick  #S(callable :fn pick  :i (:x t :y t :z t) :o (:x t :y t :z t :x t))
    :dupd  #S(callable :fn dupd  :i (:x t :y t)      :o (:x t :x t :y t))

    :drop  #S(callable :fn drop  :i (:x t)           :o ())
    :2drop #S(callable :fn 2drop :i (:x t :y t)      :o ())
    :nip   #S(callable :fn nip   :i (:x t :y t)      :o (:y t))
    :2nip  #S(callable :fn 2nip  :i (:x t :y t :z t) :o (:z t))

    :swap  #S(callable :fn swap  :i (:x t :y t)      :o (:y t :x t))
    :swapd #S(callable :fn swapd :i (:x t :y t :z t) :o (:y t :x t :z t))
    :rot   #S(callable :fn rot   :i (:x t :y t :z t) :o (:y t :z t :x t))
    :-rot  #S(callable :fn -rot  :i (:x t :y t :z t) :o (:z t :x t :y t))))

;; Sequence operators. Based on Factor naming conventions.
(defun 1list (a      ) (list a))
(defun 2list (a b    ) (list a b))
(defun 3list (a b c  ) (list a b c))
(defun 4list (a b c d) (list a b c d))

(defconstant +seq-op-callables+
  '(:cons  #S(callable :fn cons  :i (:a t :b t) :o (:cons cons))
    :1list #S(callable :fn 1list :i (:a t) :o (:list cons))
    :2list #S(callable :fn 2list :i (:a t :b t) :o (:list cons))
    :3list #S(callable :fn 3list :i (:a t :b t :c t) :o (:list cons))
    :4list #S(callable :fn 4list :i (:a t :b t :c t :d t) :o (:list cons))

    ;; Custom array operators.
    :copy-array
    #S(mopr-plug:callable :fn alexandria:copy-array
                          :i (:ref-array array)
                          :o (:new-array array))))

(defconstant +configuration+
  `((:callables ,+arith-op-callables+)
    (:callables ,+stack-op-callables+)
    (:callables ,+seq-op-callables+)))
