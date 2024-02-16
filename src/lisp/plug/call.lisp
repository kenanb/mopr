;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-plug)

(defun create-prim-call-table (table)
  (flet ((prim-fn-test-gen-xform-info (&rest args)
           (destructuring-bind (tr-array rt-array) args
             `((:prop ("xformOpOrder" :interp :uniform)
                :array :token #1A (("xformOp" "translate")
                                   ("xformOp" "rotateXYZ")))
               (:ns "xformOp"
                    (:prop "translate" :float3 ,tr-array)
                    (:prop "rotateXYZ" :double3 ,rt-array)))))
         (prim-fn-grid-extent (x y z s)
           (list (list :prop "extent"
                       :array :float3
                       (make-array '(2 3)
                                   :initial-contents
                                   `((00 00 00)
                                     (,(* s x) ,(* s y) ,(* s z)))))))
         (prim-fn-grid-fv-counts (w h)
           (list (list :prop "faceVertexCounts"
                       :array :int
                       (make-array
                        (* w h)
                        :initial-element 4))))
         (prim-fn-grid-fv-indices (w h
                                   &key
                                     (counter-clockwise-p nil)
                                   &aux
                                     (p (if counter-clockwise-p
                                            (list 0 1 (+ w 2) (+ w 1))
                                            (list 0 (+ w 1) (+ w 2) 1))))
           (list (list :prop "faceVertexIndices"
                       :array :int
                       (make-array
                        (list (* w h 4))
                        :initial-contents
                        (loop for y below h
                              nconc (loop for x below w
                                          nconc (mapcar (lambda (s) (+ x s (* y (+ 1 w)))) p)))))))
         (prim-fn-grid-points-xy (w h s)
           (list (list :prop "points"
                       :array :point3f
                       (make-array
                        (list (* (1+ h) (1+ w)) 3)
                        :initial-contents
                        (loop for y upto h
                              nconc (loop for x upto w
                                          collect (list (* s x) (* s y) (* s 0)))))))))

    (setf (gethash :grid-extent table) #'prim-fn-grid-extent)
    (setf (gethash :grid-fv-counts table) #'prim-fn-grid-fv-counts)
    (setf (gethash :grid-fv-indices table) #'prim-fn-grid-fv-indices)
    (setf (gethash :grid-points-xy table) #'prim-fn-grid-points-xy)
    (setf (gethash :test-gen-xform-info table) #'prim-fn-test-gen-xform-info)))

(defun create-data-call-table (table)
  (flet ((data-fn-test-gen-cubes (&rest args)
           (flet ((define-cube (x r prim-name)
                    (let* ((tr (list (mod x r) (floor (/ x r)) 0))
                           (rt (list 0 x x))
                           (tr-a (make-array 3 :initial-contents tr))
                           (rt-a (make-array 3 :initial-contents rt)))
                      `(:prim (,prim-name)
                              (:type mopr-ns:Cube)
                              (:prop "size" :double #0A .5)
                              (:call :test-gen-xform-info ,tr-a ,rt-a)))))
             (destructuring-bind (r) args
               (loop for x below (* r r)
                     for prim-name = (format nil "Prim_~4,'0d" x)
                     collecting (list prim-name :spec :def) into tree
                     collecting (define-cube x r prim-name) into prims
                     finally (return `((:tree ,@tree) ,@prims))))))

         (data-fn-test-tree-gen (&rest args)
           ;; TODO: We don't need args for this.
           (declare (ignore args))
           '((:tree
              ("a" :spec :class)
              ("b"
               ("d"
                ("e" :spec :over :alias :x)
                ("f" :alias :y)))
              ("c")))))

    (setf (gethash :test-gen-cubes table) #'data-fn-test-gen-cubes)
    (setf (gethash :test-tree-gen table) #'data-fn-test-tree-gen)))
