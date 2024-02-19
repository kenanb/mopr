;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

(defvar *attr-info-fv-counts*
  (make-instance 'mopr-prop:attr-info
                 :base-name "faceVertexCounts"
                 :array-p t
                 :type-key :int))

(defvar *attr-info-fv-indices*
  (make-instance 'mopr-prop:attr-info
                 :base-name "faceVertexIndices"
                 :array-p t
                 :type-key :int))

(defvar *attr-info-extent*
  (make-instance 'mopr-prop:attr-info
                 :base-name "extent"
                 :array-p t
                 :type-key :float3))

(defvar *attr-info-points*
  (make-instance 'mopr-prop:attr-info
                 :base-name "points"
                 :array-p t
                 :type-key :point3f))

;; Grid generation functions.

(defun aref-point (a2d subscript)
  (make-array 3 :displaced-to a2d
                :displaced-index-offset (array-row-major-index a2d subscript 0)))

(defun make-extent-array (min-a max-a)
  (make-array '(2 3) :initial-contents (list min-a max-a)))

(defun compute-extent (points)
  (let* ((min-a #3(0))
         (max-a #3(0)))
    (loop for p-sub below (array-dimension points 0)
          for p = (aref-point points p-sub)
          do (setf min-a (map 'vector #'min min-a p))
          do (setf max-a (map 'vector #'max max-a p)))
    (make-extent-array min-a max-a)))

(defstruct
    (point-based
     (:include mopr-prop:compound)
     (:constructor make-point-based
         (points-data
          &optional
            (extent-data (compute-extent points-data))
          &aux
            (mopr-prop::properties
             (list
              (mopr-prop:make-property
               :info *attr-info-extent*
               :data (list extent-data))
              (mopr-prop:make-property
               :info *attr-info-points*
               :data (list points-data))))))))

(defun prim-fn-grid-extent (s x y z)
  (mopr-prop:make-property
   :info *attr-info-extent*
   :data (list (make-extent-array '(00 00 00)
                                  (list (* s x) (* s y) (* s z))))))

(defun prim-fn-grid-fv-counts (w h)
  (mopr-prop:make-property
   :info *attr-info-fv-counts*
   :data (list (make-array
                (* w h)
                :initial-element 4))))

(defun prim-fn-grid-fv-indices (w h dir
                                &aux (p (if (eq dir :ccw)
                                            (list 0 1 (+ w 2) (+ w 1))
                                            (list 0 (+ w 1) (+ w 2) 1))))
  (let ((contents
          (make-array
           (list (* w h 4))
           :initial-contents
           (loop for y below h
                 nconc (loop for x below w
                             nconc (mapcar (lambda (s) (+ x s (* y (+ 1 w)))) p))))))
    (mopr-prop:make-property
     :info *attr-info-fv-indices*
     :data (list contents))))

(defun make-points-array (dims contents)
  (make-array
   (list (apply #'* (mapcar #'1+ dims)) 3)
   :initial-contents contents))

(defun prim-fn-grid-points (s dims order)
  (let* ((min-a #3(0))
         (max-a #3(0))
         (contents
           (loop for elt-d0 upto (elt dims 0)
                 nconc (loop for elt-d1 upto (elt dims 1)
                             nconc (loop for elt-d2 upto (elt dims 2)
                                         for v = (vector (* s elt-d0) (* s elt-d1) (* s elt-d2))
                                         for shuffled = (map 'vector (lambda (i) (aref v i)) order)
                                         do (setf min-a (map 'vector #'min min-a shuffled))
                                         do (setf max-a (map 'vector #'max max-a shuffled))
                                         collect shuffled)))))
    (make-point-based
     (make-points-array dims contents)
     (make-extent-array min-a max-a))))
