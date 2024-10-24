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

   ;; Generic APIs
   #:rnode-get-ynode-anchor
   #:rnode-get-rdata-options
   #:find-rnode-by-id
   #:populate-command-from-rnode

   ;; RNODE API
   #:rnode
   #:rnode-rdatas
   #:rnode-parent
   #:rnode-children

   ;; ROOT-RNODE
   #:root-rnode

   ;; VAR-RNODE
   #:var-rnode
   #:var-rnode-name-param
   #:var-rnode-aux-form-param
   #:var-rnode-val-form-param

   ;; EACH-RNODE
   #:each-rnode
   #:each-rnode-name-params
   #:each-rnode-keys-form-param
   #:each-rnode-vals-form-param

   ;; IOTA-RNODE
   #:iota-rnode
   #:iota-rnode-name-param
   #:iota-rnode-key-param
   #:iota-rnode-end-param

   ;; CALL-RNODE
   #:call-rnode
   #:call-rnode-aux-form-param
   #:call-rnode-body-form-param

   ;; PRIM-CALL-RNODE
   #:prim-call-rnode

   ;; PRIM-TYPE-RNODE
   #:prim-type-rnode
   #:prim-type-rnode-name-param

   ;; PRIM-ATTR-RNODE
   #:prim-attr-rnode
   #:prim-attr-rnode-name-param
   #:prim-attr-rnode-meta-form-param
   #:prim-attr-rnode-category-param
   #:prim-attr-rnode-type-param
   #:prim-attr-rnode-body-form-param

   ;; PRIM-META-RNODE
   #:prim-meta-rnode

   ;; PRIM-RNODE
   #:prim-rnode
   #:prim-rnode-path-form-param

   ;; TREE-RNODE
   #:tree-rnode
   #:tree-rnode-body-form-param

   ;; META-RNODE
   #:meta-rnode
   #:meta-rnode-body-form-param

   ))

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

(defgeneric rnode-get-rdata-options (node id-sub)
  (:documentation "Get the options available for the selected rdata of given node."))

(defclass rnode ()
  ((id
    :type (unsigned-byte 32)
    :initform (prog1 *rnode-id-counter* (incf *rnode-id-counter*))
    :reader rnode-id)
   (parent
    :type (or null rnode)
    :initarg :parent
    :initform nil
    :accessor rnode-parent)
   (children
    :type (vector rnode)
    :initform (make-array 0 :element-type 'rnode :adjustable t :fill-pointer 0)
    :accessor rnode-children)
   (rdatas
    :type list
    :initform nil
    :accessor rnode-rdatas)))

(defmethod rnode-get-ynode-anchor ((n rnode))
  (error (format nil "RNODE type ~A doesn't support children!" (class-name (class-of n)))))

(defmethod rnode-get-rdata-options ((node rnode) id-sub)
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
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor var-rnode-name-param)
   (aux-form-param
    :type list
    :initform nil
    :initarg :aux-form-param
    :accessor var-rnode-aux-form-param)
   (val-form-param
    :type list
    :initform nil
    :initarg :val-form-param
    :accessor var-rnode-val-form-param)))

(defmethod initialize-instance :after ((node var-rnode) &key)
  (multiple-value-bind (val-form-param-text
                        val-form-param-line-count)
      (format-form (var-rnode-val-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-0+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (rnode-get-ynode-anchor (rnode-parent node))))
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
                                :text (format nil "~S" (var-rnode-name-param node))))
           (nac1 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 6
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal1 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 7
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                                :text "AUX"
                                :bg color))
           (nai1 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 8
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                                :text (format nil "~S" (var-rnode-aux-form-param node))))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 9
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
                               :text val-form-param-text
                               :h-co val-form-param-line-count)))
      (setf (rnode-rdatas node)
            (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nar)))))

;; TODO
(defmethod rnode-get-rdata-options ((node var-rnode) id-sub)
  (case id-sub
    (1 (list "var expr-label-rdata"))
    (4 (list "var attr-label-rdata NAME"))
    (5 (list "var attr-input-rdata NAME"))
    (7 (list "var attr-label-rdata AUX FORM"))
    (8 (list "var attr-input-rdata AUX FORM"))
    (9 (list "var attr-input-rdata VAL FORM"))))

(defclass each-rnode (rnode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor each-rnode-name-param)
   (keys-form-param
    :type list
    :initform nil
    :initarg :keys-form-param
    :accessor each-rnode-keys-form-param)
   (vals-form-param
    :type list
    :initform nil
    :initarg :vals-form-param
    :accessor each-rnode-vals-form-param)))

(defmethod initialize-instance :after ((node each-rnode) &key)
  (multiple-value-bind (vals-form-param-text
                        vals-form-param-line-count)
      (format-form (each-rnode-vals-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-1+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (rnode-get-ynode-anchor (rnode-parent node))))
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
                                :text (format nil "~S" (each-rnode-name-param node))))
           (nac1 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 6
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal1 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 7
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                                :text "KEY(S)"
                                :bg color))
           (nai1 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 8
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                                :text (format nil "~S" (each-rnode-keys-form-param node))))
           (nac2 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 9
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal2 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 10
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
                                :text "VALUE(S)"
                                :bg color))
           (nai2 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 11
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
                                :text vals-form-param-text
                                :h-co vals-form-param-line-count)))
      (setf (rnode-rdatas node)
            (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nac2 nal2 nai2)))))

;; TODO
(defmethod rnode-get-rdata-options ((node each-rnode) id-sub)
  (case id-sub
    (1 (list "each expr-label-rdata"))
    (4 (list "each attr-label-rdata NAME"))
    (5 (list "each attr-input-rdata NAME"))
    (7 (list "each attr-label-rdata KEY(S)"))
    (8 (list "each attr-input-rdata KEY(S)"))
    (10 (list "each attr-label-rdata VALUE(S)"))
    (11 (list "each attr-input-rdata VALUE(S)"))))

(defclass iota-rnode (rnode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor iota-rnode-name-param)
   (key-param
    :type base-string
    :initarg :key-param
    :accessor iota-rnode-key-param)
   (end-param
    :type integer
    :initarg :end-param
    :accessor iota-rnode-end-param)))

(defmethod initialize-instance :after ((node iota-rnode) &key)
  (let* ((color mopr-def:+command-theme-expr-bg-2+)
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent (rnode-get-ynode-anchor (rnode-parent node))))
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
                              :text (format nil "~S" (iota-rnode-name-param node))))
         (nac1 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                              :id 6
                              :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
         (nal1 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                              :id 7
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                              :text "KEY"
                              :bg color))
         (nai1 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                              :id 8
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac1)
                              :text (format nil "~S" (iota-rnode-key-param node))))
         (nac2 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                              :id 9
                              :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
         (nal2 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                              :id 10
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
                              :text "END"
                              :bg color))
         (nai2 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                              :id 11
                              :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
                              :text (format nil "~S" (iota-rnode-end-param node)))))
    (setf (rnode-rdatas node)
          (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nac2 nal2 nai2))))

;; TODO
(defmethod rnode-get-rdata-options ((node iota-rnode) id-sub)
  (case id-sub
    (1 (list "iota expr-label-rdata"))
    (4 (list "iota attr-label-rdata NAME"))
    (5 (list "iota attr-input-rdata NAME"))
    (7 (list "iota attr-label-rdata KEY"))
    (8 (list "iota attr-input-rdata KEY"))
    (10 (list "iota attr-label-rdata END"))
    (11 (list "iota attr-input-rdata END"))))

(defclass call-rnode (rnode)
  ((aux-form-param
    :type list
    :initarg :aux-form-param
    :accessor call-rnode-aux-form-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor call-rnode-body-form-param)))

(defmethod initialize-instance :after ((node call-rnode) &key)
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (call-rnode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-3+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (rnode-get-ynode-anchor (rnode-parent node))))
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
                                :text "AUX"
                                :bg color))
           (nai0 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 5
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac0)
                                :text (format nil "~S" (call-rnode-aux-form-param node))))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 6
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas node)
            (list nec nel ncc nac0 nal0 nai0 nar)))))

;; TODO
(defmethod rnode-get-rdata-options ((node call-rnode) id-sub)
  (case id-sub
    (1 (list "call expr-label-rdata"))
    (4 (list "call attr-label-rdata AUX"))
    (5 (list "call attr-input-rdata AUX"))
    (6 (list "call attr-input-rdata BODY"))))

(defclass prim-call-rnode (call-rnode)
  ())

(defclass prim-type-rnode (rnode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-type-rnode-name-param)))

(defmethod initialize-instance :after ((node prim-type-rnode) &key)
  (let* ((color mopr-def:+command-theme-expr-bg-4+)
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent (rnode-get-ynode-anchor (rnode-parent node))))
         (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                             :id 1
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                             :text "TYPE"
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
                              :text (format nil "~S" (prim-type-rnode-name-param node)))))
    (setf (rnode-rdatas node)
          (list nec nel ncc nac0 nal0 nai0))))

;; TODO
(defmethod rnode-get-rdata-options ((node prim-type-rnode) id-sub)
  (case id-sub
    (1 (list "prim-type expr-label-rdata"))
    (4 (list "prim-type attr-label-rdata NAME"))
    (5 (list "prim-type attr-input-rdata NAME"))))

(defclass prim-attr-rnode (rnode)
  ((name-param
    :type (or symbol base-string)
    :initarg :name-param
    :accessor prim-attr-rnode-name-param)
   (meta-form-param
    :type list
    :initarg :meta-form-param
    :initform nil
    :accessor prim-attr-rnode-meta-form-param)
   (category-param
    :type keyword
    :initarg :category-param
    :accessor prim-attr-rnode-category-param)
   (type-param
    :type keyword
    :initarg :type-param
    :accessor prim-attr-rnode-type-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-attr-rnode-body-form-param)))

(defmethod initialize-instance :after ((node prim-attr-rnode) &key)
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (prim-attr-rnode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-8+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (rnode-get-ynode-anchor (rnode-parent node))))
           (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                               :id 1
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                               :text "ATTR"
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
                                :text (format nil "~S" (prim-attr-rnode-name-param node))))
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
                                :text (format nil "~S" (prim-attr-rnode-meta-form-param node))))
           (nac2 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 9
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal2 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 10
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
                                :text "CATEGORY"
                                :bg color))
           (nai2 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 11
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac2)
                                :text (format nil "~S" (prim-attr-rnode-category-param node))))
           (nac3 (make-instance 'mopr-ext/repr-rdata:attr-container-rdata
                                :id 12
                                :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)))
           (nal3 (make-instance 'mopr-ext/repr-rdata:attr-label-rdata
                                :id 13
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac3)
                                :text "TYPE"
                                :bg color))
           (nai3 (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                                :id 14
                                :yparent (mopr-ext/repr-rdata:rdata-ynode nac3)
                                :text (format nil "~S" (prim-attr-rnode-type-param node))))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 15
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas node)
            (list nec nel ncc
                  nac0 nal0 nai0
                  nac1 nal1 nai1
                  nac2 nal2 nai2
                  nac3 nal3 nai3
                  nar)))))

;; TODO
(defmethod rnode-get-rdata-options ((node prim-attr-rnode) id-sub)
  (case id-sub
    (1 (list "prim-attr expr-label-rdata"))
    (4 (list "prim-attr attr-label-rdata NAME"))
    (5 (list "prim-attr attr-input-rdata NAME"))
    (7 (list "prim-attr attr-label-rdata META"))
    (8 (list "prim-attr attr-input-rdata META"))
    (10 (list "prim-attr attr-label-rdata CATEGORY"))
    (11 (list "prim-attr attr-input-rdata CATEGORY"))
    (13 (list "prim-attr attr-label-rdata TYPE"))
    (14 (list "prim-attr attr-input-rdata TYPE"))
    (15 (list "prim-attr attr-input-rdata BODY"))))

(defclass prim-rnode (rnode)
  ((path-form-param
    :type list
    :initarg :path-form-param
    :accessor prim-rnode-path-form-param)))

(defmethod initialize-instance :after ((node prim-rnode) &key)
  (let* ((color mopr-def:+command-theme-expr-bg-5+)
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent (rnode-get-ynode-anchor (rnode-parent node))))
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
                              :text (format nil "~S" (prim-rnode-path-form-param node)))))
    (setf (rnode-rdatas node)
          (list nec nel ncc nac0 nal0 nai0))))

;; TODO
(defmethod rnode-get-rdata-options ((node prim-rnode) id-sub)
  (case id-sub
    (1 (list "prim expr-label-rdata"))
    (4 (list "prim attr-label-rdata PATH"))
    (5 (list "prim attr-input-rdata PATH"))
    (7 (list "prim attr-label-rdata META"))
    (8 (list "prim attr-input-rdata META"))))

(defmethod rnode-get-ynode-anchor ((n prim-rnode))
  (mopr-ext/repr-rdata:rdata-ynode (caddr (rnode-rdatas n))))

(defclass tree-rnode (rnode)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor tree-rnode-body-form-param)))

(defmethod initialize-instance :after ((node tree-rnode) &key)
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (tree-rnode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-6+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (rnode-get-ynode-anchor (rnode-parent node))))
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
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas node)
            (list nec nel ncc nar)))))

;; TODO
(defmethod rnode-get-rdata-options ((node tree-rnode) id-sub)
  (case id-sub
    (1 (list "tree expr-label-rdata"))
    (3 (list "tree attr-input-rdata BODY"))))

(defclass meta-rnode (rnode)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor meta-rnode-body-form-param)))

;; TODO : Add support for metadata handling.
(defmethod initialize-instance :after ((node meta-rnode) &key)
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (meta-rnode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-7+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (rnode-get-ynode-anchor (rnode-parent node))))
           (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                               :id 1
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                               :text "META"
                               :bg color))
           (ncc (make-instance 'mopr-ext/repr-rdata:content-container-rdata
                               :id 2
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 3
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas node)
            (list nec nel ncc nar)))))

;; TODO
(defmethod rnode-get-rdata-options ((node meta-rnode) id-sub)
  (case id-sub
    (1 (list "meta expr-label-rdata"))
    (3 (list "meta attr-input-rdata BODY"))))

(defclass prim-meta-rnode (meta-rnode)
  ())
