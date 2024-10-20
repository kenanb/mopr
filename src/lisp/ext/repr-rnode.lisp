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
(defmethod get-rdata-options ((node var-rnode) id-sub)
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
(defmethod get-rdata-options ((node each-rnode) id-sub)
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
(defmethod get-rdata-options ((node iota-rnode) id-sub)
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
(defmethod get-rdata-options ((node call-rnode) id-sub)
  (case id-sub
    (1 (list "call expr-label-rdata"))
    (4 (list "call attr-label-rdata AUX"))
    (5 (list "call attr-input-rdata AUX"))
    (6 (list "call attr-input-rdata BODY"))))

(defclass prim-rnode (rnode)
  ((path-form-param
    :type list
    :initarg :path-form-param
    :accessor prim-rnode-path-form-param)
   (meta-form-param
    :type list
    :initarg :meta-form-param
    :accessor prim-rnode-meta-form-param)))

(defmethod initialize-instance :after ((node prim-rnode) &key)
  (let* ((color mopr-def:+command-theme-expr-bg-4+)
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
                              :text (format nil "~S" (prim-rnode-path-form-param node))))
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
                              :text (format nil "~S" (prim-rnode-meta-form-param node)))))
    (setf (rnode-rdatas node)
          (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1))))

;; TODO
(defmethod get-rdata-options ((node prim-rnode) id-sub)
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
    (let* ((color mopr-def:+command-theme-expr-bg-5+)
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
(defmethod get-rdata-options ((node tree-rnode) id-sub)
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
    (let* ((color mopr-def:+command-theme-expr-bg-5+)
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
(defmethod get-rdata-options ((node meta-rnode) id-sub)
  (case id-sub
    (1 (list "meta expr-label-rdata"))
    (3 (list "meta attr-input-rdata BODY"))))
