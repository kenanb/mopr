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
  '(:add #S(callable :fn add :i (:x :y) :o (:result))
    :sub #S(callable :fn sub :i (:x :y) :o (:result))
    :mul #S(callable :fn mul :i (:x :y) :o (:result))
    :div #S(callable :fn div :i (:x :y) :o (:result))
    :neg #S(callable :fn neg :i (:x   ) :o (:result))
    :inc #S(callable :fn inc :i (:x   ) :o (:result))
    :dec #S(callable :fn dec :i (:x   ) :o (:result))))

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
  '(:dup   #S(callable :fn dup   :i (:x)       :o (:x :x))
    :2dup  #S(callable :fn 2dup  :i (:x :y)    :o (:x :y :x :y))
    :over  #S(callable :fn over  :i (:x :y)    :o (:x :y :x))
    :2over #S(callable :fn 2over :i (:x :y :z) :o (:x :y :z :x :y))
    :pick  #S(callable :fn pick  :i (:x :y :z) :o (:x :y :z :x))
    :dupd  #S(callable :fn dupd  :i (:x :y)    :o (:x :x :y))

    :drop  #S(callable :fn drop  :i (:x)       :o ())
    :2drop #S(callable :fn 2drop :i (:x :y)    :o ())
    :nip   #S(callable :fn nip   :i (:x :y)    :o (:y))
    :2nip  #S(callable :fn 2nip  :i (:x :y :z) :o (:z))

    :swap  #S(callable :fn swap  :i (:x :y)    :o (:y :x))
    :swapd #S(callable :fn swapd :i (:x :y :z) :o (:y :x :z))
    :rot   #S(callable :fn rot   :i (:x :y :z) :o (:y :z :x))
    :-rot  #S(callable :fn -rot  :i (:x :y :z) :o (:z :x :y))))

;; Sequence operators. Based on Factor naming conventions.
(defun 1list (a      ) (list a))
(defun 2list (a b    ) (list a b))
(defun 3list (a b c  ) (list a b c))
(defun 4list (a b c d) (list a b c d))

(defconstant +seq-op-callables+
  '(:cons  #S(callable :fn cons  :i (:a :b) :o (:cons))
    :1list #S(callable :fn 1list :i (:a) :o (:list))
    :2list #S(callable :fn 2list :i (:a :b) :o (:list))
    :3list #S(callable :fn 3list :i (:a :b :c) :o (:list))
    :4list #S(callable :fn 4list :i (:a :b :c :d) :o (:list))

    ;; Custom array operators.
    :copy-array
    #S(mopr-plug:callable :fn alexandria:copy-array
                          :i (:ref-array)
                          :o (:new-array))))

(defconstant +configuration+
  `((:callables ,+arith-op-callables+)
    (:callables ,+stack-op-callables+)
    (:callables ,+seq-op-callables+)))
