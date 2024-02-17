;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

;; Grid generation functions.

(defun prim-fn-grid-extent (x y z s)
  `((:prop "extent"
     :array :float3
     ,(make-array '(2 3)
                  :initial-contents
                  `((00 00 00)
                    ,(list (* s x) (* s y) (* s z)))))))

(defun prim-fn-grid-fv-counts (w h)
  `((:prop "faceVertexCounts"
     :array :int
     ,(make-array
       (* w h)
       :initial-element 4))))

(defun prim-fn-grid-fv-indices (w h
                                &key (counter-clockwise-p nil)
                                &aux (p (if counter-clockwise-p
                                            (list 0 1 (+ w 2) (+ w 1))
                                            (list 0 (+ w 1) (+ w 2) 1))))
  `((:prop "faceVertexIndices"
     :array :int
     ,(make-array
       (list (* w h 4))
       :initial-contents
       (loop for y below h
             nconc (loop for x below w
                         nconc (mapcar (lambda (s) (+ x s (* y (+ 1 w)))) p)))))))

(defun prim-fn-grid-points (s dims &optional (order #(2 1 0)))
  (let* ((min-a #3(0))
         (max-a #3(0))
         (points
           (make-array
            (list (apply #'* (mapcar #'1+ dims)) 3)
            :initial-contents
            (loop for elt-d0 upto (elt dims 0)
                  nconc (loop for elt-d1 upto (elt dims 1)
                              nconc (loop for elt-d2 upto (elt dims 2)
                                          for v = (vector (* s elt-d0) (* s elt-d1) (* s elt-d2))
                                          for shuffled = (map 'vector (lambda (i) (aref v i)) order)
                                          do (setf min-a (map 'vector #'min min-a shuffled))
                                          do (setf max-a (map 'vector #'max max-a shuffled))
                                          collect shuffled)))))
         (extent (make-array '(2 3) :initial-contents (list min-a max-a))))
    `((:prop "points"
       :array :point3f
       ,points)
      (:prop "extent"
       :array :float3
       ,extent))))
