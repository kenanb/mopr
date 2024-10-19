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
   #:populate-command-options
   #:destruct-command-options
   #:apply-command-option
   #:with-layout-settings
   #:testing))

(in-package :mopr-ext/repr)

(defvar *fill-column* 70)

(defvar *debug-mode* t)

(defvar *enable-call* nil)

;;
;;; Utilities
;;

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

(defun format-form (form margin)
  (let ((*print-pretty* t)
        (*print-right-margin* margin))
    (let* ((text (format nil "~S" form))
           (line-count (1+ (count #\newline text))))
      (values text line-count))))

(defmacro %dim (ynode accessor)
  (case accessor
    (:left `(yoga-fun:node-layout-get-left ,ynode))
    (:right `(yoga-fun:node-layout-get-right ,ynode))
    (:top `(yoga-fun:node-layout-get-top ,ynode))
    (:bottom `(yoga-fun:node-layout-get-bottom ,ynode))
    (:width `(yoga-fun:node-layout-get-width ,ynode))
    (:height `(yoga-fun:node-layout-get-height ,ynode))))

(defun recursive-get-left (ynode)
  (if (autowrap:wrapper-null-p ynode) 0
      (+ (%dim ynode :left)
         (recursive-get-left (yoga-fun:node-get-parent ynode)))))

(defun recursive-get-top (ynode)
  (if (autowrap:wrapper-null-p ynode) 0
      (+ (%dim ynode :top)
         (recursive-get-top (yoga-fun:node-get-parent ynode)))))

(defmacro %set-values (obj accessor &rest key-value-plist)
  `(progn
     ,@(loop for (k v . rest) on key-value-plist by #'cddr
             collecting `(setf (plus-c:c-ref ,obj ,@accessor ,k) ,v))))

;;
;;; RDATA and Generic Functions
;;

(defgeneric rdata-command-type (node)
  (:documentation "Command type to assign for the node."))

(defgeneric populate-command-from-rdata (node c)
  (:documentation "Populate command to represent the behaviour of the given node."))

(defclass rdata ()
  ((id
    :type (unsigned-byte 32)
    :initarg :id
    :initform 0
    :reader rdata-id)
   (ynode
    :type yoga-def:node-ref
    :initform (yoga-fun:node-new)
    :reader rdata-ynode)))

(defmethod initialize-instance :after ((node rdata) &key (yparent nil))
  (when yparent
    (with-slots (ynode) node
      (yoga-fun:node-insert-child yparent ynode (yoga-fun:node-get-child-count yparent)))))

(defmethod rdata-command-type ((n rdata))
  mopr-def:+command-type-base+)

(defmethod populate-command-from-rdata ((n rdata) c &aux (y (rdata-ynode n)))
  (%set-values c (mopr-def:combined-command :base)
               :c-type (rdata-command-type n)
               :id-sub (rdata-id n)
               :x (recursive-get-left y)
               :y (recursive-get-top y)
               :w (%dim y :width)
               :h (%dim y :height)))

;;
;;; HIDDEN-RDATA
;;

(defclass hidden-rdata (rdata)
  ())

(defmethod populate-command-from-rdata ((n hidden-rdata) c)
  (error "Invalid node passed to populate-command-from-rdata!"))

;;
;;; ROOT-CONTAINER-RDATA
;;

(defclass root-container-rdata (rdata)
  ())

(defmethod initialize-instance :after ((node root-container-rdata) &key)
  (with-slots (ynode) node
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-column+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 8.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-row+ 8.0f0)))

(defmethod rdata-command-type ((n root-container-rdata))
  mopr-def:+command-type-draw-root-container+)

;;
;;; EXPR-CONTAINER-RDATA
;;

(defclass expr-container-rdata (rdata)
  ())

(defmethod initialize-instance :after ((node expr-container-rdata) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-row+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 6.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-column+ 6.0f0)))

(defmethod rdata-command-type ((n expr-container-rdata))
  mopr-def:+command-type-draw-expr-container+)

;;
;;; EXPR-LABEL-RDATA
;;

(defclass expr-label-rdata (rdata)
  ((bg
    :initarg :bg
    :type mopr-def:command-theme
    :initform mopr-def:+command-theme-none+
    :reader rdata-bg)
   (text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader rdata-text)))

(defmethod initialize-instance :after ((node expr-label-rdata) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    (yoga-fun:node-style-set-width ynode 60)
    (yoga-fun:node-style-set-min-height ynode 32)))

(defmethod rdata-command-type ((n expr-label-rdata))
  mopr-def:+command-type-draw-expr-label+)

(defmethod populate-command-from-rdata ((n expr-label-rdata) c)
  (%set-values c (mopr-def:combined-command :draw-expr-label)
               :bg (rdata-bg n)
               :text (autowrap:alloc-string (rdata-text n)))
  (call-next-method))

;;
;;; CONTENT-CONTAINER-RDATA
;;

(defclass content-container-rdata (hidden-rdata)
  ())

(defmethod initialize-instance :after ((node content-container-rdata) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-column+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 0.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-row+ 6.0f0)))

;;
;;; ATTR-CONTAINER-RDATA
;;

(defclass attr-container-rdata (hidden-rdata)
  ())

(defmethod initialize-instance :after ((node attr-container-rdata) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-row+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 0.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-column+ 4.0f0)))

(defclass attr-label-rdata (rdata)
  ((bg
    :initarg :bg
    :type mopr-def:command-theme
    :initform mopr-def:+command-theme-none+
    :reader rdata-bg)
   (text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader rdata-text)))

(defmethod initialize-instance :after ((node attr-label-rdata) &key (h-co 1))
  (with-slots (ynode text) node
    (yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    (yoga-fun:node-style-set-min-width ynode (+ 16 (* (length text) 10)))
    (yoga-fun:node-style-set-min-height ynode (+ 16 (* h-co 16)))))

(defmethod rdata-command-type ((n attr-label-rdata))
  mopr-def:+command-type-draw-attr-label+)

(defmethod populate-command-from-rdata ((n attr-label-rdata) c)
  (%set-values c (mopr-def:combined-command :draw-attr-label)
               :bg (rdata-bg n)
               :text (autowrap:alloc-string (rdata-text n)))
  (call-next-method))

(defclass attr-input-rdata (rdata)
  ((text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader rdata-text)))

(defmethod initialize-instance :after ((node attr-input-rdata) &key (h-co 1))
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    (yoga-fun:node-style-set-min-width ynode 200)
    (yoga-fun:node-style-set-min-height ynode (+ 16 (* h-co 16)))))

(defmethod rdata-command-type ((n attr-input-rdata))
  mopr-def:+command-type-draw-attr-input+)

(defmethod populate-command-from-rdata ((n attr-input-rdata) c)
  (%set-values c (mopr-def:combined-command :draw-attr-input)
               :text (autowrap:alloc-string (rdata-text n)))
  (call-next-method))

;;
;;; RNODE and Generic Functions
;;

(defvar *root-rnode* nil)

;; Zero value is reserved for "no selection".
(defvar *rnode-id-counter* 1)

(defgeneric rnode-get-ynode-anchor (node)
  (:documentation "Get the ynode that should contain child ynodes."))

(defgeneric get-rdata-options (node id-sub)
  (:documentation "Get the options available for the selected rdata of given node."))

(defclass rnode ()
  ((id
    :type (unsigned-byte 32)
    :initform (prog1 *rnode-id-counter* (incf *rnode-id-counter*))
    :reader rnode-id)
   (rdatas
    :type list
    :initform nil
    :accessor rnode-rdatas)
   (parent
    :type (or null rnode)
    :initform nil
    :accessor rnode-parent)
   (children
    :type (vector rnode)
    :initform (make-array 0 :element-type 'rnode :adjustable t :fill-pointer 0)
    :accessor rnode-children)))

(defmethod initialize-instance :after ((node rnode) &key rparent)
  (setf (rnode-parent node) rparent))

(defmethod rnode-get-ynode-anchor ((n rnode))
  (error (format nil "RNODE type ~A doesn't support children!" (class-name (class-of n)))))

(defmethod get-rdata-options ((node rnode) id-sub)
  nil)

(defun find-rnode-by-id (n id)
  (if (eql (rnode-id n) id) n
      (loop for c across (rnode-children n) for x = (find-rnode-by-id c id) if x return x)))

(defun populate-command-from-rnode (n c)
  (%set-values c (mopr-def:combined-command :base)
               :id (rnode-id n)))

(defclass root-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node root-rnode) &key)
  (let* ((nrc (make-instance 'root-container-rdata
                             :id 0)))
    (setf (rnode-rdatas node)
          (list nrc))))

(defmethod rnode-get-ynode-anchor ((n root-rnode))
  (rdata-ynode (car (rnode-rdatas n))))

(defclass var-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node var-rnode) &key rparent form
                                       &aux
                                         (yparent (rnode-get-ynode-anchor rparent))
                                         (var-name (car form))
                                         (var-aux (cadr form))
                                         (frest (cddr form)))
  (multiple-value-bind (text-body line-count-body)
      (format-form frest *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-0+)
           (nec (make-instance 'expr-container-rdata
                               :id 0
                               :yparent yparent))
           (nel (make-instance 'expr-label-rdata
                               :id 1
                               :yparent (rdata-ynode nec)
                               :text "VAR"
                               :bg color))
           (ncc (make-instance 'content-container-rdata
                               :id 2
                               :yparent (rdata-ynode nec)))
           (nac0 (make-instance 'attr-container-rdata
                                :id 3
                                :yparent (rdata-ynode ncc)))
           (nal0 (make-instance 'attr-label-rdata
                                :id 4
                                :yparent (rdata-ynode nac0)
                                :text "NAME"
                                :bg color))
           (nai0 (make-instance 'attr-input-rdata
                                :id 5
                                :yparent (rdata-ynode nac0)
                                :text (format nil "~S" var-name)))
           (nac1 (make-instance 'attr-container-rdata
                                :id 6
                                :yparent (rdata-ynode ncc)))
           (nal1 (make-instance 'attr-label-rdata
                                :id 7
                                :yparent (rdata-ynode nac1)
                                :text "LET"
                                :bg color))
           (nai1 (make-instance 'attr-input-rdata
                                :id 8
                                :yparent (rdata-ynode nac1)
                                :text (format nil "~S" var-aux)))
           (nar (make-instance 'attr-input-rdata
                               :id 9
                               :yparent (rdata-ynode ncc)
                               :text text-body
                               :h-co line-count-body)))
      (setf (rnode-rdatas node)
            (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nar)))))

;; TODO
(defmethod get-rdata-options ((node var-rnode) id-sub)
  (case id-sub
    (1 (list "var expr-label-rdata"))
    (4 (list "var attr-label-rdata NAME"))
    (5 (list "var attr-input-rdata NAME"))
    (7 (list "var attr-label-rdata LET"))
    (8 (list "var attr-input-rdata LET"))
    (9 (list "var attr-input-rdata CONTENT"))))

(defclass each-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node each-rnode) &key rparent form
                                       &aux
                                         (yparent (rnode-get-ynode-anchor rparent))
                                         (name (car form))
                                         (arg-aggrs (cadr form))
                                         (val-aggrs (cddr form)))
  (multiple-value-bind (text-val-aggrs line-count-val-aggrs)
      (format-form val-aggrs *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-1+)
           (nec (make-instance 'expr-container-rdata
                               :id 0
                               :yparent yparent))
           (nel (make-instance 'expr-label-rdata
                               :id 1
                               :yparent (rdata-ynode nec)
                               :text "EACH"
                               :bg color))
           (ncc (make-instance 'content-container-rdata
                               :id 2
                               :yparent (rdata-ynode nec)))
           (nac0 (make-instance 'attr-container-rdata
                                :id 3
                                :yparent (rdata-ynode ncc)))
           (nal0 (make-instance 'attr-label-rdata
                                :id 4
                                :yparent (rdata-ynode nac0)
                                :text "NAME"
                                :bg color))
           (nai0 (make-instance 'attr-input-rdata
                                :id 5
                                :yparent (rdata-ynode nac0)
                                :text (format nil "~S" name)))
           (nac1 (make-instance 'attr-container-rdata
                                :id 6
                                :yparent (rdata-ynode ncc)))
           (nal1 (make-instance 'attr-label-rdata
                                :id 7
                                :yparent (rdata-ynode nac1)
                                :text "ARG-AGGR(S)"
                                :bg color))
           (nai1 (make-instance 'attr-input-rdata
                                :id 8
                                :yparent (rdata-ynode nac1)
                                :text (format nil "~S" arg-aggrs)))
           (nac2 (make-instance 'attr-container-rdata
                                :id 9
                                :yparent (rdata-ynode ncc)))
           (nal2 (make-instance 'attr-label-rdata
                                :id 10
                                :yparent (rdata-ynode nac2)
                                :text "VAL-AGGR(S)"
                                :bg color))
           (nai2 (make-instance 'attr-input-rdata
                                :id 11
                                :yparent (rdata-ynode nac2)
                                :text (format nil "~S" val-aggrs))))
      (setf (rnode-rdatas node)
            (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nac2 nal2 nai2)))))

;; TODO
(defmethod get-rdata-options ((node each-rnode) id-sub)
  (case id-sub
    (1 (list "each expr-label-rdata"))
    (4 (list "each attr-label-rdata NAME"))
    (5 (list "each attr-input-rdata NAME"))
    (7 (list "each attr-label-rdata ARG-AGGR(S)"))
    (8 (list "each attr-input-rdata ARG-AGGR(S)"))
    (10 (list "each attr-label-rdata VAL-AGGR(S)"))
    (11 (list "each attr-input-rdata VAL-AGGR(S)"))
    (12 (list "each attr-input-rdata CONTENT"))))

(defclass iota-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node iota-rnode) &key rparent form
                                       &aux
                                         (yparent (rnode-get-ynode-anchor rparent))
                                         (name (car form))
                                         (arg-aggr (cadr form))
                                         (val-aggr (caddr form)))
  (let* ((color mopr-def:+command-theme-expr-bg-2+)
         (nec (make-instance 'expr-container-rdata
                             :id 0
                             :yparent yparent))
         (nel (make-instance 'expr-label-rdata
                             :id 1
                             :yparent (rdata-ynode nec)
                             :text "IOTA"
                             :bg color))
         (ncc (make-instance 'content-container-rdata
                             :id 2
                             :yparent (rdata-ynode nec)))
         (nac0 (make-instance 'attr-container-rdata
                              :id 3
                              :yparent (rdata-ynode ncc)))
         (nal0 (make-instance 'attr-label-rdata
                              :id 4
                              :yparent (rdata-ynode nac0)
                              :text "NAME"
                              :bg color))
         (nai0 (make-instance 'attr-input-rdata
                              :id 5
                              :yparent (rdata-ynode nac0)
                              :text (format nil "~S" name)))
         (nac1 (make-instance 'attr-container-rdata
                              :id 6
                              :yparent (rdata-ynode ncc)))
         (nal1 (make-instance 'attr-label-rdata
                              :id 7
                              :yparent (rdata-ynode nac1)
                              :text "ARG-AGGR"
                              :bg color))
         (nai1 (make-instance 'attr-input-rdata
                              :id 8
                              :yparent (rdata-ynode nac1)
                              :text (format nil "~S" arg-aggr)))
         (nac2 (make-instance 'attr-container-rdata
                              :id 9
                              :yparent (rdata-ynode ncc)))
         (nal2 (make-instance 'attr-label-rdata
                              :id 10
                              :yparent (rdata-ynode nac2)
                              :text "VAL-AGGR"
                              :bg color))
         (nai2 (make-instance 'attr-input-rdata
                              :id 11
                              :yparent (rdata-ynode nac2)
                              :text (format nil "~S" val-aggr))))
    (setf (rnode-rdatas node)
          (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nac2 nal2 nai2))))

;; TODO
(defmethod get-rdata-options ((node iota-rnode) id-sub)
  (case id-sub
    (1 (list "iota expr-label-rdata"))
    (4 (list "iota attr-label-rdata NAME"))
    (5 (list "iota attr-input-rdata NAME"))
    (7 (list "iota attr-label-rdata ARG-AGGR"))
    (8 (list "iota attr-input-rdata ARG-AGGR"))
    (10 (list "iota attr-label-rdata VAL-AGGR"))
    (11 (list "iota attr-input-rdata VAL-AGGR"))
    (12 (list "iota attr-input-rdata CONTENT"))))

(defclass call-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node call-rnode) &key rparent form
                                       &aux
                                         (yparent (rnode-get-ynode-anchor rparent))
                                         (call-aux (car form))
                                         (call-body (cdr form)))
  (multiple-value-bind (text-call-body line-count-call-body)
      (format-form call-body *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-3+)
           (nec (make-instance 'expr-container-rdata
                               :id 0
                               :yparent yparent))
           (nel (make-instance 'expr-label-rdata
                               :id 1
                               :yparent (rdata-ynode nec)
                               :text "CALL"
                               :bg color))
           (ncc (make-instance 'content-container-rdata
                               :id 2
                               :yparent (rdata-ynode nec)))
           (nac0 (make-instance 'attr-container-rdata
                                :id 3
                                :yparent (rdata-ynode ncc)))
           (nal0 (make-instance 'attr-label-rdata
                                :id 4
                                :yparent (rdata-ynode nac0)
                                :text "LET"
                                :bg color))
           (nai0 (make-instance 'attr-input-rdata
                                :id 5
                                :yparent (rdata-ynode nac0)
                                :text (format nil "~S" call-aux)))
           (nar (make-instance 'attr-input-rdata
                               :id 6
                               :yparent (rdata-ynode ncc)
                               :text text-call-body
                               :h-co line-count-call-body)))
      (setf (rnode-rdatas node)
            (list nec nel ncc nac0 nal0 nai0 nar)))))

;; TODO
(defmethod get-rdata-options ((node call-rnode) id-sub)
  (case id-sub
    (1 (list "call expr-label-rdata"))
    (4 (list "call attr-label-rdata LET"))
    (5 (list "call attr-input-rdata LET"))
    (6 (list "call attr-input-rdata CONTENT"))))

(defclass prim-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node prim-rnode) &key rparent form
                                       &aux
                                         (yparent (rnode-get-ynode-anchor rparent))
                                         (fpath (car form))
                                         (fmeta (cadr form)))
  (let* ((color mopr-def:+command-theme-expr-bg-4+)
         (nec (make-instance 'expr-container-rdata
                             :id 0
                             :yparent yparent))
         (nel (make-instance 'expr-label-rdata
                             :id 1
                             :yparent (rdata-ynode nec)
                             :text "PRIM"
                             :bg color))
         (ncc (make-instance 'content-container-rdata
                             :id 2
                             :yparent (rdata-ynode nec)))
         (nac0 (make-instance 'attr-container-rdata
                              :id 3
                              :yparent (rdata-ynode ncc)))
         (nal0 (make-instance 'attr-label-rdata
                              :id 4
                              :yparent (rdata-ynode nac0)
                              :text "PATH"
                              :bg color))
         (nai0 (make-instance 'attr-input-rdata
                              :id 5
                              :yparent (rdata-ynode nac0)
                              :text (format nil "~S" fpath)))
         (nac1 (make-instance 'attr-container-rdata
                              :id 6
                              :yparent (rdata-ynode ncc)))
         (nal1 (make-instance 'attr-label-rdata
                              :id 7
                              :yparent (rdata-ynode nac1)
                              :text "META"
                              :bg color))
         (nai1 (make-instance 'attr-input-rdata
                              :id 8
                              :yparent (rdata-ynode nac1)
                              :text (format nil "~S" fmeta))))
    (setf (rnode-rdatas node)
          (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1))))

;; TODO
(defmethod get-rdata-options ((node prim-rnode) id-sub)
  (case id-sub
    (1 (list "prim expr-label-rdata"))
    (4 (list "prim attr-label-rdata PATH"))
    (5 (list "prim attr-input-rdata PATH"))
    (7 (list "prim attr-label-rdata META"))
    (8 (list "prim attr-input-rdata META"))
    (9 (list "prim attr-input-rdata CONTENT"))))

(defmethod rnode-get-ynode-anchor ((n prim-rnode))
  (rdata-ynode (caddr (rnode-rdatas n))))

(defclass tree-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node tree-rnode) &key rparent form
                                       &aux
                                         (yparent (rnode-get-ynode-anchor rparent)))
  (let* ((color mopr-def:+command-theme-expr-bg-5+)
         (nec (make-instance 'expr-container-rdata
                             :id 0
                             :yparent yparent))
         (nel (make-instance 'expr-label-rdata
                             :id 1
                             :yparent (rdata-ynode nec)
                             :text "TREE"
                             :bg color))
         (ncc (make-instance 'content-container-rdata
                             :id 2
                             :yparent (rdata-ynode nec)))
         (nar (make-instance 'attr-input-rdata
                             :id 3
                             :yparent (rdata-ynode ncc)
                             :text (format-form form *fill-column*))))
    (setf (rnode-rdatas node)
          (list nec nel ncc nar))))

;; TODO
(defmethod get-rdata-options ((node tree-rnode) id-sub)
  (case id-sub
    (1 (list "tree expr-label-rdata"))
    (3 (list "tree attr-input-rdata CONTENT"))))

(defclass meta-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node meta-rnode) &key rparent form
                                       &aux
                                         (yparent (rnode-get-ynode-anchor rparent)))
  (declare (ignore form))
  (let* ((color mopr-def:+command-theme-expr-bg-6+)
         (nec (make-instance 'expr-container-rdata
                             :id 0
                             :yparent yparent))
         (nel (make-instance 'expr-label-rdata
                             :id 1
                             :yparent (rdata-ynode nec)
                             :text "META"
                             :color color))
         (ncc (make-instance 'content-container-rdata
                             :id 2
                             :yparent (rdata-ynode nec)))
         ;; TODO : Add support for metadata handling.
         )
    (setf (rnode-rdatas node)
          (list nec nel ncc))))

;; TODO
(defmethod get-rdata-options ((node meta-rnode) id-sub)
  (case id-sub
    (1 (list "meta expr-label-rdata"))))

;;
;;; Form Handlers
;;

(defun handle-var-form (rparent form)
  ;; (format t "~%Called handle-var-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'var-rnode :rparent rparent :form form)
                      (rnode-children rparent)))

(defun handle-each-form (rparent form)
  ;; (format t "~%Called handle-each-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'each-rnode :rparent rparent :form form)
                      (rnode-children rparent)))

(defun handle-iota-form (rparent form)
  ;; (format t "~%Called handle-iota-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'iota-rnode :rparent rparent :form form)
                      (rnode-children rparent)))

(defun handle-call-form (rparent form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'call-rnode :rparent rparent :form form)
                      (rnode-children rparent)))

(defun handle-prim-form (rparent form)
  ;; (format t "~%Called handle-prim-form!~%: ~S~%" form)
  (let* ((pn (make-instance 'prim-rnode :rparent rparent :form form)))
    (vector-push-extend pn (rnode-children rparent))
    (loop for l in (cddr form)
          for i from 0
          for fn = (case (car l)
                     ;; TODO : Handle other forms.
                     (:call   #'handle-call-form)
                     (:|call| #'handle-call-form))
          do (if fn
                 (funcall fn pn (cdr l))
                 (unknown-form-error (car l) :debug)))))

(defun handle-tree-form (rparent form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'tree-rnode :rparent rparent :form form)
                      (rnode-children rparent)))

(defun handle-meta-form (rparent form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'meta-rnode :rparent rparent :form form)
                      (rnode-children rparent)))

(defun handle-data-subforms (rparent subforms)
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
        do (if fn
               (funcall fn rparent (cdr l))
               (unknown-form-error (car l) :debug))))

;;
;;; Trivial Vector Type Backed by a C Array
;;

(defclass cvec ()
  ((ctype
    :type symbol
    :initarg :ctype
    :initform (error "Ctype must be specified!")
    :accessor cvec-ctype)
   (size
    :type (unsigned-byte 32)
    :initarg :size
    :initform 0
    :accessor cvec-size)
   (idx
    :type (unsigned-byte 32)
    :initform 0
    :accessor cvec-idx)
   (wrapper
    :accessor cvec-wrapper)))

(defmethod initialize-instance :after ((vec cvec) &key)
  (setf (cvec-wrapper vec)
        (autowrap:alloc (cvec-ctype vec) (cvec-size vec))))

(defun cvec-get-incrementing-counter (vec)
  (prog1
      (autowrap:c-aref (cvec-wrapper vec)
                       (cvec-idx vec)
                       (cvec-ctype vec))
    (incf (cvec-idx vec))))

;;
;;; Top-Level API and Macros
;;

(defmacro with-repr-variables ((&key
                                  (enable-call nil))
                               &body body)
  `(let* ((*enable-call* ,enable-call))
     ,@body))

(defun build-repr-call-enabled (usds-data)
  (mopr-info:with-registry (:supported-cases '(:upcase))
    (mopr-plug:with-configuration ()
      (with-repr-variables (:enable-call t)
        (let* ((n (make-instance 'root-rnode))) (handle-data-subforms n usds-data) n)))))

(defmacro with-layout-settings (&body body)
  `(float-features:with-float-traps-masked (:invalid)
     ,@body))

(defun %populate-commands-recursive (n wcmds)
  (loop for rd in (rnode-rdatas n)
        unless (typep rd 'hidden-rdata)
          do (let ((cmd (cvec-get-incrementing-counter wcmds)))
               (populate-command-from-rnode n cmd)
               (populate-command-from-rdata rd cmd)))
  (loop for c across (rnode-children n)
        do (%populate-commands-recursive c wcmds)))

(defun %count-visible-rdata-recursive (n)
  (+ (count-if-not (lambda (x) (typep x 'hidden-rdata)) (rnode-rdatas n))
     (loop for c across (rnode-children n)
           summing (%count-visible-rdata-recursive c))))

(defun populate-command-queue (cmd-queue usds-data)
  (with-layout-settings
      ;; TODO : Implement cleanup.
      (setf *root-rnode* (build-repr-call-enabled usds-data))
    (let* ((pixels-w (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-w))
           ;; (pixels-h (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-h))
           (root-yn (rdata-ynode (car (rnode-rdatas *root-rnode*))))
           (wcmds
             (make-instance 'cvec
                            :ctype 'mopr-def:combined-command
                            :size (%count-visible-rdata-recursive *root-rnode*))))

      (yoga-fun:node-calculate-layout root-yn
                                      pixels-w
                                      yoga-def:+undefined+ ;; pixels-h
                                      yoga-def:+direction-ltr+)

      (%populate-commands-recursive *root-rnode* wcmds)

      (%set-values cmd-queue (mopr-def:command-queue)
                   :nof-commands (cvec-size wcmds)
                   :commands (autowrap:ptr (cvec-wrapper wcmds))
                   ;; Adjust height to the actual "used" height.
                   :pixels-h (%dim root-yn :height))

      ;; TODO : Defer.
      (yoga-fun:node-free-recursive root-yn))))

;; NOTE: Free calls are made from the same module the allocations were made from,
;;       to avoid possible issues with multiple malloc implementations in runtime.
(defun destruct-command-queue (cmd-queue-ptr
                               &aux
                                 (cmd-queue (autowrap:wrap-pointer
                                             cmd-queue-ptr 'mopr-def:command-queue)))
  (let* ((cmd-count (plus-c:c-ref cmd-queue mopr-def:command-queue :nof-commands))
         (commands (plus-c:c-ref cmd-queue mopr-def:command-queue :commands)))

    (loop for i below cmd-count
          for c = (autowrap:c-aref commands i 'mopr-def:combined-command)
          for c-type = (plus-c:c-ref c mopr-def:combined-command :base :c-type)

          ;; TODO: Formalize cleanup.
          ;;
          ;; NOTE: c-ref implements some convenience functionality based on the
          ;; last field.  However, in order to free the C string, we do need the
          ;; pointer.  So we dereference and reference in the end, to inhibit
          ;; foreign-string last-field convenience.
          do (case c-type
               (mopr-def:+command-type-draw-expr-label+
                (autowrap:free (plus-c:c-ref c mopr-def:combined-command
                                             :draw-expr-label
                                             :text plus-c:* plus-c:&)))
               (mopr-def:+command-type-draw-attr-label+
                (autowrap:free (plus-c:c-ref c mopr-def:combined-command
                                             :draw-attr-label
                                             :text plus-c:* plus-c:&)))
               (mopr-def:+command-type-draw-attr-input+
                (autowrap:free (plus-c:c-ref c mopr-def:combined-command
                                             :draw-attr-input
                                             :text plus-c:* plus-c:&)))))

    (autowrap:free commands)

    (%set-values cmd-queue (mopr-def:command-queue) :nof-commands 0 :commands (autowrap:ptr nil))))

(defun destruct-command-options (cmd-options-ptr
                                 &aux
                                   (cmd-options (autowrap:wrap-pointer
                                                 cmd-options-ptr 'mopr-def:command-options)))
  (let* ((opt-count (plus-c:c-ref cmd-options mopr-def:command-options :nof-options))
         (options (plus-c:c-ref cmd-options mopr-def:command-options :options)))

    (loop for i below opt-count
          do (autowrap:free (autowrap:c-aref options i :pointer)))

    (autowrap:free options)

    (%set-values cmd-options (mopr-def:command-options) :nof-options 0 :options (autowrap:ptr nil))))

(defun populate-command-options (cmd-options-ptr id id-sub
                                 &aux
                                   (cmd-options (autowrap:wrap-pointer
                                                 cmd-options-ptr 'mopr-def:command-options)))
  (let* ((n (find-rnode-by-id *root-rnode* id))
         (opts (get-rdata-options n id-sub))
         (nof-opts (length opts))
         (vopts (autowrap:alloc :pointer nof-opts)))

    (loop for o in opts for i from 0
          do (setf (autowrap:c-aref vopts i :pointer) (autowrap:alloc-string o)))

    (%set-values cmd-options (mopr-def:command-options)
                 :nof-options nof-opts
                 :options (autowrap:ptr vopts))))

(defun apply-command-option (id id-sub id-opt)
  (let* ((n (find-rnode-by-id *root-rnode* id))
         (opts (get-rdata-options n id-sub))
         (idx (1- id-opt)))
    (format t "APPLIED OPTION: ~A~%" (nth idx opts))))

;;
;;; TESTING
;;

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
