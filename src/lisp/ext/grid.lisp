;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;


(in-package :cl-user)

(defpackage :mopr-ext/grid
  (:use #:cl #:mopr)
  (:export
   #:aref-point
   #:make-extent-array
   #:make-points-array
   #:compute-extent))

(in-package :mopr-ext/grid)

;; Call table generation.

(defconstant +prim-callables+
  '(:grid-extent
    #S(mopr-plug:callable :fn prim-fn-grid-extent
                          :i (:size :x :y :z)
                          :o (:prop-entry))

    :grid-fv-counts
    #S(mopr-plug:callable :fn prim-fn-grid-fv-counts
                          :i (:w :h)
                          :o (:prop-entry))

    :grid-fv-indices
    #S(mopr-plug:callable :fn prim-fn-grid-fv-indices
                          :i (:w :h :dir)
                          :o (:prop-entry))

    :grid-points
    #S(mopr-plug:callable :fn prim-fn-grid-points
                          :i (:size :dims :order)
                          :o (:point-based))))

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

(defstruct
    (point-based
     (:include mopr-sgt:data-group)
     (:constructor make-point-based
         (points-data
          &optional
            (extent-data (compute-extent points-data))
          &aux
            (mopr-sgt::data
             (list
              (mopr-sgt:make-prop-entry
               :info (mopr-info:get-prop-info-for-schema :isa :Mesh :extent)
               :data (list extent-data))
              (mopr-sgt:make-prop-entry
               :info (mopr-info:get-prop-info-for-schema :isa :Mesh :points)
               :data (list points-data))))))))

(defun prim-fn-grid-extent (size x y z
                            &aux (s (coerce size 'single-float)))
  (mopr-sgt:make-prop-entry
   :info (mopr-info:get-prop-info-for-schema :isa :Mesh :extent)
   :data (list (make-extent-array '(0.0 0.0 0.0)
                                  (list (* s x) (* s y) (* s z))))))

(defun prim-fn-grid-fv-counts (w h)
  (mopr-sgt:make-prop-entry
   :info (mopr-info:get-prop-info-for-schema :isa :Mesh :faceVertexCounts)
   :data (list (make-array
                (* w h)
                :element-type '(signed-byte 32)
                :initial-element 4))))

(defun prim-fn-grid-fv-indices (w h dir
                                &aux (p (if (eq dir :ccw)
                                            (list 0 1 (+ w 2) (+ w 1))
                                            (list 0 (+ w 1) (+ w 2) 1))))
  (let ((contents
          (make-array
           (list (* w h 4))
           :element-type '(signed-byte 32)
           :initial-contents
           (loop for y below h
                 nconc (loop for x below w
                             nconc (mapcar (lambda (s) (+ x s (* y (+ 1 w)))) p))))))
    (mopr-sgt:make-prop-entry
     :info (mopr-info:get-prop-info-for-schema :isa :Mesh :faceVertexIndices)
     :data (list contents))))

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
    (make-point-based
     (make-points-array dims contents)
     (make-extent-array min-a max-a))))
