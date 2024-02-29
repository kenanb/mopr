;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

(defun dup (x)
  (values x x))

(defun swap (x y)
  (values y x))

(defconstant +configuration+
  '((:generic-callables
     (:dup
      #S(callable :fn dup
                  :i (:x)
                  :o (:x :x))
      :swap
      #S(callable :fn swap
                  :i (:x :y)
                  :o (:y :x))))))
