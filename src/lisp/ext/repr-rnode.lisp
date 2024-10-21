;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr-rnode
  (:import-from :mopr)
  (:import-from :mopr-ext/repr-shared
                #:multiple-set-c-ref)
  (:import-from :mopr-ext/repr-rdata)
  (:use :cl)
  (:export
   #:rnode-get-ynode-anchor
   #:get-rdata-options
   #:rnode-rdatas
   #:rnode-children
   #:find-rnode-by-id
   #:populate-command-from-rnode
   #:root-rnode
   #:var-rnode
   #:each-rnode
   #:iota-rnode
   #:call-rnode
   #:prim-rnode
   #:tree-rnode
   #:meta-rnode))

(in-package :mopr-ext/repr-rnode)

(defvar *fill-column* 70)

;;
;;; Utilities
;;

(defun format-form (form margin)
  (let ((*print-pretty* t)
        (*print-right-margin* margin))
    (let* ((text (format nil "~S" form))
           (line-count (1+ (count #\newline text))))
      (values text line-count))))

;;
;;; RNODE and Generic Functions
;;

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
  (multiple-set-c-ref c (mopr-def:combined-command :base)
                      :id (rnode-id n)))

(defclass root-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node root-rnode) &key)
  (let* ((nrc (make-instance 'mopr-ext/repr-rdata:root-container-rdata
                             :id 0)))
    (setf (rnode-rdatas node)
          (list nrc))))

(defmethod rnode-get-ynode-anchor ((n root-rnode))
  (mopr-ext/repr-rdata:rdata-ynode (car (rnode-rdatas n))))

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
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent yparent))
           (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                               :id 1
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                               :text "VAR"
                               :bg color))
           (ncc (make-instance 'mopr-ext/repr-rdata:content-container-rdata
                               :id 2
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)))
           (nac0 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 3
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal0 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 4
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                                :text "NAME"
                                :bg color))
           (nai0 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 5
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                                :text (format nil "~S" var-name)))
           (nac1 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 6
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal1 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 7
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                                :text "LET"
                                :bg color))
           (nai1 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 8
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                                :text (format nil "~S" var-aux)))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 9
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
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
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent yparent))
           (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                               :id 1
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                               :text "EACH"
                               :bg color))
           (ncc (make-instance 'mopr-ext/repr-rdata:content-container-rdata
                               :id 2
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)))
           (nac0 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 3
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal0 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 4
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                                :text "NAME"
                                :bg color))
           (nai0 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 5
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                                :text (format nil "~S" name)))
           (nac1 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 6
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal1 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 7
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                                :text "ARG-AGGR(S)"
                                :bg color))
           (nai1 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 8
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                                :text (format nil "~S" arg-aggrs)))
           (nac2 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 9
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal2 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 10
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
                                :text "VAL-AGGR(S)"
                                :bg color))
           (nai2 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 11
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
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
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent yparent))
         (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                             :id 1
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                             :text "IOTA"
                             :bg color))
         (ncc (make-instance 'mopr-ext/repr-rdata:content-container-rdata
                             :id 2
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)))
         (nac0 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                              :id 3
                              :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
         (nal0 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                              :id 4
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                              :text "NAME"
                              :bg color))
         (nai0 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                              :id 5
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                              :text (format nil "~S" name)))
         (nac1 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                              :id 6
                              :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
         (nal1 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                              :id 7
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                              :text "ARG-AGGR"
                              :bg color))
         (nai1 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                              :id 8
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                              :text (format nil "~S" arg-aggr)))
         (nac2 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                              :id 9
                              :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
         (nal2 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                              :id 10
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
                              :text "VAL-AGGR"
                              :bg color))
         (nai2 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                              :id 11
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
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
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent yparent))
           (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                               :id 1
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                               :text "CALL"
                               :bg color))
           (ncc (make-instance 'mopr-ext/repr-rdata:content-container-rdata
                               :id 2
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)))
           (nac0 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 3
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal0 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 4
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                                :text "LET"
                                :bg color))
           (nai0 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 5
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                                :text (format nil "~S" call-aux)))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 6
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
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
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent yparent))
         (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                             :id 1
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                             :text "PRIM"
                             :bg color))
         (ncc (make-instance 'mopr-ext/repr-rdata:content-container-rdata
                             :id 2
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)))
         (nac0 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                              :id 3
                              :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
         (nal0 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                              :id 4
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                              :text "PATH"
                              :bg color))
         (nai0 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                              :id 5
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                              :text (format nil "~S" fpath)))
         (nac1 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                              :id 6
                              :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
         (nal1 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                              :id 7
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                              :text "META"
                              :bg color))
         (nai1 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                              :id 8
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
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
  (mopr-ext/repr-rdata:rdata-ynode (caddr (rnode-rdatas n))))

(defclass tree-rnode (rnode)
  ())

(defmethod initialize-instance :after ((node tree-rnode) &key rparent form
                                       &aux
                                         (yparent (rnode-get-ynode-anchor rparent)))
  (let* ((color mopr-def:+command-theme-expr-bg-5+)
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent yparent))
         (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                             :id 1
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                             :text "TREE"
                             :bg color))
         (ncc (make-instance 'mopr-ext/repr-rdata:content-container-rdata
                             :id 2
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)))
         (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                             :id 3
                             :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
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
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent yparent))
         (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                             :id 1
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                             :text "META"
                             :color color))
         (ncc (make-instance 'mopr-ext/repr-rdata:content-container-rdata
                             :id 2
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)))
         ;; TODO : Add support for metadata handling.
         )
    (setf (rnode-rdatas node)
          (list nec nel ncc))))

;; TODO
(defmethod get-rdata-options ((node meta-rnode) id-sub)
  (case id-sub
    (1 (list "meta expr-label-rdata"))))
