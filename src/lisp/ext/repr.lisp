;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr
  (:import-from :mopr)
  (:import-from :plus-c) ; Depend on system.
  (:import-from :float-features) ; Depend on system.
  (:use :cl)
  (:export
   #:populate-command-queue
   #:destruct-command-queue
   #:with-layout-settings
   #:testing))

(in-package :mopr-ext/repr)

(defvar *fill-column* 75)

(defvar *debug-mode* t)

(defvar *enable-call* nil)

(defun unknown-form-error (form action)
  (format t "
[ERROR] Cannot handle form.
[  -  ] FORM: ~A
[  -  ] ACTION: ~A
"
          form
          (case action
            (:skip "Skipping.")
            (:debug  (if *debug-mode*
                         "Debug mode enabled: Will error."
                         "Debug mode disabled: Skipping."))
            (:error  "Will error.")
            (otherwise (error "Coding error. Unknown message action."))))
  (when (and (eq action :debug) *debug-mode*)
    (error "Cannot handle form: ~S~%" form)))

(defclass repr-node ()
  ((id
    :initarg :id
    :type integer
    :initform 0
    :reader repr-node-id)
   (color
    :initarg :color
    :type (simple-vector 4)
    :initform #(255 255 255 255)
    :reader repr-node-color)
   (text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader repr-node-text)
   (ynode
    :type yoga-def:node-ref
    :initform (yoga-fun:node-new)
    :reader repr-node-ynode)))

(defmethod initialize-instance :after ((node repr-node) &key (yparent nil))
  (when yparent
    (with-slots (ynode) node
      (yoga-fun:node-insert-child yparent ynode (yoga-fun:node-get-child-count yparent)))))

(defclass repr-node-root (repr-node)
  ((color :initform #(250 250 200 100))))

(defmethod initialize-instance :after ((node repr-node-root) &key)
  (with-slots (ynode) node
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-column+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 10.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-row+ 10.0f0)))

(defclass repr-node-top-level-container (repr-node)
  ((color :initform #(255 255 255 50))))

(defmethod initialize-instance :after ((node repr-node-top-level-container) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-row+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 5.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-column+ 5.0f0)))

(defclass repr-node-form-title (repr-node) ())

(defmethod initialize-instance :after ((node repr-node-form-title) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    (yoga-fun:node-style-set-width ynode 40)
    (yoga-fun:node-style-set-min-height ynode 20)))

(defclass repr-node-content-form (repr-node)
  ((color :initform #(0 0 0 0))))

(defmethod initialize-instance :after ((node repr-node-content-form) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-column+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 0.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-row+ 5.0f0)))

(defclass repr-node-content-attr (repr-node) ())

(defmethod initialize-instance :after ((node repr-node-content-attr) &key (h-co 1))
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    (yoga-fun:node-style-set-min-width ynode 200)
    (yoga-fun:node-style-set-min-height ynode (+ 10 (* h-co 14)))))

(defclass repr-node-content-body (repr-node) ())

(defmethod initialize-instance :after ((node repr-node-content-body) &key (h-co 1))
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    (yoga-fun:node-style-set-min-width ynode 200)
    (yoga-fun:node-style-set-min-height ynode (+ 10 (* h-co 14)))))

(defun format-form (form margin)
  (let ((*print-pretty* t)
        (*print-right-margin* margin))
    (let* ((text (format nil "~S" form))
           (line-count (1+ (count #\newline text))))
      (values text line-count))))

(defun handle-var-form (yparent form
                        &aux
                          (var-name (car form))
                          (var-aux (cadr form))
                          (frest (cddr form)))
  ;; (format t "~%Called handle-var-form!~%: ~S~%" form)
  (multiple-value-bind (text-body line-count-body)
      (format-form frest *fill-column*)
    (let* ((color #(100 200 200 50))
           (node (make-instance 'repr-node-top-level-container :yparent yparent))
           (nt (make-instance 'repr-node-form-title
                              :yparent (repr-node-ynode node)
                              :text "VAR"
                              :color color))
           (nc (make-instance 'repr-node-content-form
                              :yparent (repr-node-ynode node)))
           (nca0 (make-instance 'repr-node-content-attr
                                :yparent (repr-node-ynode nc)
                                :text (format nil "NAME: ~S" var-name)
                                :color color))
           (nca1 (make-instance 'repr-node-content-attr
                                :yparent (repr-node-ynode nc)
                                :text (format nil "AUX: ~S" var-aux)
                                :color color))
           (ncb (make-instance 'repr-node-content-body
                               :yparent (repr-node-ynode nc)
                               :text text-body
                               :color color
                               :h-co line-count-body)))
      (list node nt nc nca0 nca1 ncb))))

(defun handle-each-form (yparent form
                         &aux
                           (name (car form))
                           (arg-aggrs (cadr form))
                           (val-aggrs (cddr form)))
  ;; (format t "~%Called handle-each-form!~%: ~S~%" form)
  (multiple-value-bind (text-val-aggrs line-count-val-aggrs)
      (format-form val-aggrs *fill-column*)
    (let* ((color #(0 255 200 50))
           (node (make-instance 'repr-node-top-level-container :yparent yparent))
           (nt (make-instance 'repr-node-form-title
                              :yparent (repr-node-ynode node)
                              :text "EACH"
                              :color color))
           (nc (make-instance 'repr-node-content-form
                              :yparent (repr-node-ynode node)))
           (nca0 (make-instance 'repr-node-content-attr
                                :yparent (repr-node-ynode nc)
                                :text (format nil "NAME: ~S" name)
                                :color color))
           (nca1 (make-instance 'repr-node-content-attr
                                :yparent (repr-node-ynode nc)
                                :text (format nil "ARG-AGGR(S): ~S" arg-aggrs)
                                :color color))
           (nca2 (make-instance 'repr-node-content-attr
                                :yparent (repr-node-ynode nc)
                                :text (format nil "VAL-AGGR(S): ~S" text-val-aggrs)
                                :color color
                                :h-co line-count-val-aggrs)))
      (list node nt nc nca0 nca1 nca2))))

(defun handle-iota-form (yparent form
                         &aux
                           (name (car form))
                           (arg-aggr (cadr form))
                           (val-aggr (caddr form)))
  ;; (format t "~%Called handle-iota-form!~%: ~S~%" form)
  (let* ((color #(0 200 255 50))
         (node (make-instance 'repr-node-top-level-container :yparent yparent))
         (nt (make-instance 'repr-node-form-title
                            :yparent (repr-node-ynode node)
                            :text "IOTA"
                            :color color))
         (nc (make-instance 'repr-node-content-form
                            :yparent (repr-node-ynode node)))
         (nca0 (make-instance 'repr-node-content-attr
                              :yparent (repr-node-ynode nc)
                              :text (format nil "NAME: ~S" name)
                              :color color))
         (nca1 (make-instance 'repr-node-content-attr
                              :yparent (repr-node-ynode nc)
                              :text (format nil "ARG-AGGR: ~S" arg-aggr)
                              :color color))
         (nca2 (make-instance 'repr-node-content-attr
                              :yparent (repr-node-ynode nc)
                              :text (format nil "VAL-AGGR: ~S" val-aggr)
                              :color color)))
    (list node nt nc nca0 nca1 nca2)))

(defun handle-call-form (yparent form
                         &aux
                           (call-aux (car form))
                           (call-body (cdr form)))
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (multiple-value-bind (text-call-body line-count-call-body)
      (format-form call-body *fill-column*)
    (let* ((color #(150 150 200 50))
           (node (make-instance 'repr-node-top-level-container :yparent yparent))
           (nt (make-instance 'repr-node-form-title
                              :yparent (repr-node-ynode node)
                              :text "CALL"
                              :color color))
           (nc (make-instance 'repr-node-content-form
                              :yparent (repr-node-ynode node)))
           (nca0 (make-instance 'repr-node-content-attr
                                :yparent (repr-node-ynode nc)
                                :text (format nil "AUX: ~S" call-aux)
                                :color color))
           (ncb (make-instance 'repr-node-content-body
                               :yparent (repr-node-ynode nc)
                               :text text-call-body
                               :color color
                               :h-co line-count-call-body)))
      (list node nt nc nca0 ncb))))

(defun handle-prim-form (yparent form
                         &aux
                           (fpath (car form))
                           (fmeta (cadr form))
                           (frest (cddr form)))
  ;; (format t "~%Called handle-prim-form!~%: ~S~%" form)
  (let* ((color #(200 100 200 50))
         (node (make-instance 'repr-node-top-level-container :yparent yparent))
         (nt (make-instance 'repr-node-form-title
                            :yparent (repr-node-ynode node)
                            :text "PRIM"
                            :color color))
         (nc (make-instance 'repr-node-content-form
                            :yparent (repr-node-ynode node)))
         (nca0 (make-instance 'repr-node-content-attr
                              :yparent (repr-node-ynode nc)
                              :text (format nil "PATH: ~S" fpath)
                              :color color))
         (nca1 (make-instance 'repr-node-content-attr
                              :yparent (repr-node-ynode nc)
                              :text (format nil "META: ~S" fmeta)
                              :color color))
         (nested-nodes
           (loop for l in frest
                 for i from 0
                 for fn = (case (car l)
                            ;; TODO : Handle other forms.
                            (:call   #'handle-call-form)
                            (:|call| #'handle-call-form))
                 nconc (if fn
                           (funcall fn (repr-node-ynode nc) (cdr l))
                           (unknown-form-error (car l) :debug)))))
    (concatenate 'list (list node nt nc nca0 nca1) nested-nodes)))

(defun handle-tree-form (yparent form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (let* ((color #(0 0 200 50))
         (node (make-instance 'repr-node-top-level-container :yparent yparent))
         (nt (make-instance 'repr-node-form-title
                            :yparent (repr-node-ynode node)
                            :text "TREE"
                            :color color))
         (nc (make-instance 'repr-node-content-form
                            :yparent (repr-node-ynode node)))
         (nca0 (make-instance 'repr-node-content-attr
                              :yparent (repr-node-ynode nc)
                              :text (format-form form *fill-column*)
                              :color color)))
    (list node nt nc nca0)))

(defun handle-meta-form (yparent form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)
  (declare (ignore form))
  (let* ((color #(0 0 0 50))
         (node (make-instance 'repr-node-top-level-container :yparent yparent))
         (nt (make-instance 'repr-node-form-title
                            :yparent (repr-node-ynode node)
                            :text "META"
                            :color color))
         (nc (make-instance 'repr-node-content-form
                            :yparent (repr-node-ynode node)))
         ;; TODO : Add support for metadata handling.
         )
    (list node nt nc)))

(defun handle-data-subforms (yparent subforms)
  ;; (format t "~%Called handle-data-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for i from 0
        for fn = (case (car l)
                   (:var    #'handle-var-form)
                   (:|var|  #'handle-var-form)
                   (:each   #'handle-each-form)
                   (:|each| #'handle-each-form)
                   (:iota   #'handle-iota-form)
                   (:|iota| #'handle-iota-form)
                   (:call   #'handle-call-form)
                   (:|call| #'handle-call-form)
                   (:meta   #'handle-meta-form)
                   (:|meta| #'handle-meta-form)
                   (:tree   #'handle-tree-form)
                   (:|tree| #'handle-tree-form)
                   (:prim   #'handle-prim-form)
                   (:|prim| #'handle-prim-form))
        nconc (if fn
                  (funcall fn yparent (cdr l))
                  (unknown-form-error (car l) :debug))))

(defmacro with-repr-variables ((&key
                                  (enable-call nil))
                               &body body)
  `(let* ((*enable-call* ,enable-call))
     ,@body))

(defun build-repr-call-enabled (usds-data)
  (mopr-info:with-registry (:supported-cases '(:upcase))
    (mopr-plug:with-configuration ()
      (with-repr-variables (:enable-call t)
        (let* ((root-node (make-instance 'repr-node-root)))
          (cons root-node (handle-data-subforms (repr-node-ynode root-node) usds-data)))))))

(defmacro with-layout-settings (&body body)
  `(float-features:with-float-traps-masked (:invalid)
     ,@body))

(defmacro %dim (node accessor)
  (case accessor
    (:left `(yoga-fun:node-layout-get-left ,node))
    (:right `(yoga-fun:node-layout-get-right ,node))
    (:top `(yoga-fun:node-layout-get-top ,node))
    (:bottom `(yoga-fun:node-layout-get-bottom ,node))
    (:width `(yoga-fun:node-layout-get-width ,node))
    (:height `(yoga-fun:node-layout-get-height ,node))))

(defmacro %set-values (obj accessor &rest key-value-plist)
  `(progn
     ,@(loop for (k v . rest) on key-value-plist by #'cddr
             collecting `(setf (plus-c:c-ref ,obj ,@accessor ,k) ,v))))

(defun recursive-get-left (ynode)
  (if (autowrap:wrapper-null-p ynode) 0
      (+ (%dim ynode :left)
         (recursive-get-left (yoga-fun:node-get-parent ynode)))))

(defun recursive-get-top (ynode)
  (if (autowrap:wrapper-null-p ynode) 0
      (+ (%dim ynode :top)
         (recursive-get-top (yoga-fun:node-get-parent ynode)))))

(defun populate-command-queue (cmd-queue usds-data)
  (with-layout-settings
      (let* ((pixels-w (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-w))
             ;; (pixels-h (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-h))
             (nodes (build-repr-call-enabled usds-data))
             (cmd-count (length nodes))
             (ynode (repr-node-ynode (car nodes)))
             (commands (autowrap:alloc 'mopr-def:combined-command cmd-count)))

        (yoga-fun:node-calculate-layout ynode
                                        pixels-w
                                        yoga-def:+undefined+ ;; pixels-h
                                        yoga-def:+direction-ltr+)

        (loop for n in nodes
              for y = (repr-node-ynode n)
              for i to cmd-count
              for c = (autowrap:c-aref commands i 'mopr-def:combined-command)
              do (%set-values c (mopr-def:combined-command :draw-rect)
                              :c-type mopr-def:+command-type-draw-rect+
                              :x (recursive-get-left y)
                              :y (recursive-get-top y)
                              :w (%dim y :width)
                              :h (%dim y :height)
                              :rounding 5.0f0
                              :text (autowrap:alloc-string (repr-node-text n)))
              do (%set-values c (mopr-def:combined-command :draw-rect :col)
                              0 (aref (repr-node-color n) 0)
                              1 (aref (repr-node-color n) 1)
                              2 (aref (repr-node-color n) 2)
                              3 (aref (repr-node-color n) 3)))

        (%set-values cmd-queue (mopr-def:command-queue)
                     :nof-commands cmd-count
                     :commands (autowrap:ptr commands)
                     ;; Adjust height to the actual "used" height.
                     :pixels-h (%dim ynode :height))

        (yoga-fun:node-free-recursive ynode))))

;; NOTE: Free calls are made from the same module the allocations were made from,
;;       to avoid possible issues with multiple malloc implementations in runtime.
(defun destruct-command-queue (cmd-queue-ptr
                               &aux
                                 (cmd-queue (autowrap:wrap-pointer
                                             cmd-queue-ptr 'mopr-def:command-queue)))
  (let* ((cmd-count (plus-c:c-ref cmd-queue mopr-def:command-queue :nof-commands))
         (commands (plus-c:c-ref cmd-queue mopr-def:command-queue :commands)))

    (loop for i to cmd-count
          for c = (autowrap:c-aref commands i 'mopr-def:combined-command)
          for c-type = (plus-c:c-ref c mopr-def:combined-command :base :c-type)
          do (when (eql c-type mopr-def:+command-type-draw-rect+)
               ;; c-ref implements some convenience functionality based on the last field.
               ;; However, in order to free the C string, we do need the pointer.  So we
               ;; dereference and reference in the end, to inhibit foreign-string last-field
               ;; convenience.
               (autowrap::free (plus-c:c-ref c mopr-def:combined-command
                                             :draw-rect :text plus-c:* plus-c:&))))

    (autowrap:free commands)

    (%set-values cmd-queue (mopr-def:command-queue) :nof-commands 0 :commands (autowrap:ptr nil))))

;; TESTING

(defun dummy-yoga-layout (pixels-w pixels-h)
  ;; For disabling pixel rounding:
  ;; YGConfigRef config = YGConfigNew( );
  ;; YGConfigSetPointScaleFactor( config, 0.0f );
  (let ((root (yoga-fun:node-new))
        (c0 (yoga-fun:node-new))
        (c1 (yoga-fun:node-new))
        (c1c0 (yoga-fun:node-new))
        (c1c1 (yoga-fun:node-new))
        (c1c2 (yoga-fun:node-new)))
    (yoga-fun:node-style-set-flex-direction root yoga-def:+flex-direction-column+)
    (yoga-fun:node-style-set-width root pixels-w)
    (yoga-fun:node-style-set-height root pixels-h)
    (yoga-fun:node-style-set-padding root yoga-def:+edge-all+ 10.0f0)

    (yoga-fun:node-style-set-flex-grow c0 1.0f0)
    (yoga-fun:node-style-set-margin c0 yoga-def:+edge-all+ 10.0f0)

    (yoga-fun:node-style-set-flex-grow c1 1.0f0)
    (yoga-fun:node-style-set-margin c1 yoga-def:+edge-all+ 10.0f0)

    (yoga-fun:node-style-set-flex-direction c1 yoga-def:+flex-direction-row+)

    (yoga-fun:node-style-set-flex-grow c1c0 1.0f0)
    (yoga-fun:node-style-set-margin c1c0 yoga-def:+edge-all+ 10.0f0)
    (yoga-fun:node-style-set-flex-grow c1c1 1.0f0)
    (yoga-fun:node-style-set-margin c1c1 yoga-def:+edge-all+ 10.0f0)
    (yoga-fun:node-style-set-flex-grow c1c2 1.0f0)
    (yoga-fun:node-style-set-margin c1c2 yoga-def:+edge-all+ 10.0f0)

    (yoga-fun:node-insert-child root c0 0)
    (yoga-fun:node-insert-child root c1 0)
    (yoga-fun:node-insert-child c1 c1c0 0)
    (yoga-fun:node-insert-child c1 c1c1 1)
    (yoga-fun:node-insert-child c1 c1c2 2)

    ;; (yoga-fun:node-calculate-layout root pixels-w pixels-h yoga-def:+direction-ltr+))
    (yoga-fun:node-calculate-layout root
                                    yoga-def:+undefined+
                                    yoga-def:+undefined+
                                    yoga-def:+direction-ltr+)

    (prog1 (%dim c1 :top)
      ;; (format t "~{~a~}"
      ;;         (list
      ;;          (list (%dim c0 :left)
      ;;                (%dim c0 :top))
      ;;          (list (+ (%dim c0 :left) (%dim c0 :width))
      ;;                (+ (%dim c0 :top) (%dim c0 :height)))))
      (yoga-fun:node-free-recursive root))))

(defun testing ()
  (with-layout-settings
      (dummy-yoga-layout 640.0f0 480.0f0)))
