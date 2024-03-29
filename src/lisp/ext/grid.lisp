;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;


(in-package :cl-user)

(defpackage :mopr-ext/grid
  (:import-from :mopr)
  (:use :cl)
  (:export
   #:aref-point
   #:make-extent-array
   #:make-points-array
   #:compute-extent))

(in-package :mopr-ext/grid)

;; Call table generation.

(deftype non-default-timecode ()
  "Type definition for timecode that's not Default time."
  '(or float integer))

(deftype boundable-extent ()
  "Type definition for boundable geometry extent."
  '(simple-array single-float (2 3)))

(deftype point-based-points ()
  "Type definition for point based geometry points."
  '(simple-array single-float))

(deftype mesh-fv-counts ()
  "Type definition for mesh face-vertex counts."
  '(simple-array (signed-byte 32)))

(deftype mesh-fv-indices ()
  "Type definition for mesh face-vertex indices."
  '(simple-array (signed-byte 32)))

(defconstant +callables+
  '(:compute-extent
    #S(mopr-plug:callable :fn compute-extent
                          :i (:points point-based-points)
                          :o (:extent boundable-extent))

    :grid-fv-counts
    #S(mopr-plug:callable :fn prim-fn-grid-fv-counts
                          :i (:w fixnum
                              :h fixnum)
                          :o (:fv-counts mesh-fv-counts))

    :grid-fv-indices
    #S(mopr-plug:callable :fn prim-fn-grid-fv-indices
                          :i (:w fixnum
                              :h fixnum
                              :dir keyword)
                          :o (:fv-indices mesh-fv-indices))

    :grid-extent
    #S(mopr-plug:callable :fn prim-fn-grid-extent
                          :i (:size real
                              :x fixnum
                              :y fixnum
                              :z fixnum)
                          :o (:extent boundable-extent))

    :grid-points
    #S(mopr-plug:callable :fn prim-fn-grid-points
                          :i (:size real
                              :dims list
                              :order simple-vector)
                          :o (:points point-based-points
                              :extent boundable-extent))

    :grid-oscillate-y
    #S(mopr-plug:callable :fn prim-fn-grid-oscillate-y
                          :i (:points point-based-points
                              :dim real
                              :len real
                              :val real)
                          :o (:points point-based-points))

    :grid-sine-y
    #S(mopr-plug:callable :fn prim-fn-grid-sine-y
                          :i (:points point-based-points
                              :len real
                              :time non-default-timecode)
                          :o (:points point-based-points))))

;; Grid generation functions.

(defun aref-point (a2d subscript)
  (make-array 3 :displaced-to a2d
                :element-type 'single-float
                :displaced-index-offset (array-row-major-index a2d subscript 0)))

(defun make-extent-array (min-a max-a)
  (make-array '(2 3)
              :element-type 'single-float
              :initial-contents (list min-a max-a)))

(defun compute-extent (points)
  (let* ((min-a #3(0.0))
         (max-a #3(0.0)))
    (loop for p-sub below (array-dimension points 0)
          for p = (aref-point points p-sub)
          do (setf min-a (map 'vector #'min min-a p))
          do (setf max-a (map 'vector #'max max-a p)))
    (make-extent-array min-a max-a)))

(defun prim-fn-grid-extent (size x y z
                            &aux (s (coerce size 'single-float)))
  (make-extent-array '(0.0 0.0 0.0)
                     (list (* s x) (* s y) (* s z))))

(defun prim-fn-grid-fv-counts (w h)
  (make-array
   (* w h)
   :element-type '(signed-byte 32)
   :initial-element 4))

(defun prim-fn-grid-fv-indices (w h dir
                                &aux (p (if (eq dir :ccw)
                                            (list 0 1 (+ w 2) (+ w 1))
                                            (list 0 (+ w 1) (+ w 2) 1))))
  (make-array
   (list (* w h 4))
   :element-type '(signed-byte 32)
   :initial-contents
   (loop for y below h
         nconc (loop for x below w
                     nconc (mapcar (lambda (s) (+ x s (* y (+ 1 w)))) p)))))

(defun make-points-array (dims contents)
  (make-array
   (list (apply #'* (mapcar #'1+ dims)) 3)
   :element-type 'single-float
   :initial-contents contents))

(defun prim-fn-grid-points (size dims order)
  (let* ((s (coerce size 'single-float))
         (min-a #3(0.0))
         (max-a #3(0.0))
         (contents
           (loop for elt-d0 upto (elt dims 0)
                 nconc (loop for elt-d1 upto (elt dims 1)
                             nconc (loop for elt-d2 upto (elt dims 2)
                                         for v = (vector (* s elt-d0)
                                                         (* s elt-d1)
                                                         (* s elt-d2))
                                         for shuffled = (map 'vector (lambda (i) (aref v i)) order)
                                         do (setf min-a (map 'vector #'min min-a shuffled))
                                         do (setf max-a (map 'vector #'max max-a shuffled))
                                         collect shuffled)))))
    (values
     (make-points-array dims contents)
     (make-extent-array min-a max-a))))

(defun prim-fn-grid-oscillate-y (p-data dim len val
                                 &aux
                                   (len-t (coerce len 'single-float))
                                   (val-t (coerce val 'single-float)))
  (loop for p-sub below (array-dimension p-data 0)
        for p = (aref-point p-data p-sub)
        unless (eq 0 (mod p-sub dim))
          do (setf val-t (if (eq 0.0 val-t) len-t 0.0))
        end
        do (setf (aref p 1) val-t))
  p-data)

(defun prim-fn-grid-sine-y (p-data len time)
  (loop for p-sub below (array-dimension p-data 0)
        for p = (aref-point p-data p-sub)
        for val = (* len (sin (+ (aref p 0) (/ time 10))))
        do (setf (aref p 1) (coerce val 'single-float)))
  p-data)
