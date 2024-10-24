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
   #:enode-get-ynode-anchor
   #:enode-get-rdata-options
   #:find-enode-by-id
   #:populate-command-from-enode

   ;; RNODE API
   #:rnode
   #:rnode-id
   #:rnode-rdatas

   ;; ENODE API
   #:enode
   #:enode-rdatas
   #:enode-parent
   #:enode-children

   ;; ROOT-ENODE
   #:root-enode

   ;; VAR-ENODE
   #:var-enode
   #:var-enode-name-param
   #:var-enode-aux-form-param
   #:var-enode-val-form-param

   ;; EACH-ENODE
   #:each-enode
   #:each-enode-name-params
   #:each-enode-keys-form-param
   #:each-enode-vals-form-param

   ;; IOTA-ENODE
   #:iota-enode
   #:iota-enode-name-param
   #:iota-enode-key-param
   #:iota-enode-end-param

   ;; CALL-ENODE
   #:call-enode
   #:call-enode-aux-form-param
   #:call-enode-body-form-param

   ;; PRIM-CALL-ENODE
   #:prim-call-enode

   ;; PRIM-TYPE-ENODE
   #:prim-type-enode
   #:prim-type-enode-name-param

   ;; PRIM-NS-ENODE
   #:prim-ns-enode
   #:prim-ns-enode-name-param

   ;; PRIM-ATTR-ENODE
   #:prim-attr-enode
   #:prim-attr-enode-name-param
   #:prim-attr-enode-meta-form-param
   #:prim-attr-enode-category-param
   #:prim-attr-enode-type-param
   #:prim-attr-enode-body-form-param

   ;; PRIM-REL-ENODE
   #:prim-rel-enode
   #:prim-rel-enode-name-param
   #:prim-rel-enode-meta-form-param
   #:prim-rel-enode-body-form-param

   ;; PRIM-META-ENODE
   #:prim-meta-enode

   ;; PRIM-ENODE
   #:prim-enode
   #:prim-enode-path-form-param

   ;; TREE-ENODE
   #:tree-enode
   #:tree-enode-body-form-param

   ;; META-ENODE
   #:meta-enode
   #:meta-enode-body-form-param

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
;;; ENODE and Generic Functions
;;

;; Zero value is reserved for "no selection".
(defvar *rnode-id-counter* 1)

(defclass rnode ()
  ((id
    :type (unsigned-byte 32)
    :initform (prog1 *rnode-id-counter* (incf *rnode-id-counter*))
    :reader rnode-id)
   (rdatas
    :type list
    :initform nil
    :accessor rnode-rdatas)))

(defgeneric enode-get-ynode-anchor (node)
  (:documentation "Get the ynode that should contain child ynodes."))

(defgeneric enode-get-rdata-options (node id-sub)
  (:documentation "Get the options available for the selected rdata of given node."))

(defgeneric enode-initialize-extension (node ext)
  (:documentation "Populate the rnode bound to enode."))

(defclass enode ()
  ((children
    :type (vector enode)
    :initform (make-array 0 :element-type 'enode :adjustable t :fill-pointer 0)
    :accessor enode-children)
   (parent
    :type (or null enode)
    :initarg :parent
    :initform nil
    :accessor enode-parent)
   (extensions
    :type list
    :initarg :extensions
    :initform nil
    :accessor enode-extensions)))

(defmethod initialize-instance :after ((node enode) &key)
  (loop for ext in (enode-extensions node)
        do (enode-initialize-extension node ext)))

(defun enode-find-extension (node typ)
  (loop for ext in (enode-extensions node) when (typep ext typ)
        return ext))

(defun enode-id (node &aux (rn (enode-find-extension node 'rnode)))
  (rnode-id rn))

(defun enode-rdatas (node &aux (rn (enode-find-extension node 'rnode)))
  (rnode-rdatas rn))

(defun set-enode-rdatas (node rdatas-val &aux (rn (enode-find-extension node 'rnode)))
  (unless rn (error "Cannot set rdatas for an enode that has no rnode bound!"))
  (setf (rnode-rdatas rn) rdatas-val))

(defsetf enode-rdatas set-enode-rdatas)

(defmethod enode-get-ynode-anchor ((n enode))
  (error (format nil "ENODE type ~A doesn't support children!" (class-name (class-of n)))))

(defmethod enode-get-rdata-options ((node enode) id-sub)
  nil)

(defun find-enode-by-id (n id)
  (if (eql (enode-id n) id) n
      (loop for c across (enode-children n) for x = (find-enode-by-id c id) if x return x)))

(defun populate-command-from-enode (n c)
  (multiple-set-c-ref c (mopr-def:combined-command :base)
                      :id (enode-id n)))

(defclass root-enode (enode)
  ())

(defmethod enode-initialize-extension ((node root-enode) (ext rnode))
  (let* ((nrc (make-instance 'mopr-ext/repr-rdata:root-container-rdata
                             :id 0)))
    (setf (rnode-rdatas ext)
          (list nrc))))

(defmethod enode-get-ynode-anchor ((n root-enode))
  (mopr-ext/repr-rdata:rdata-ynode (car (enode-rdatas n))))

(defclass var-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor var-enode-name-param)
   (aux-form-param
    :type list
    :initform nil
    :initarg :aux-form-param
    :accessor var-enode-aux-form-param)
   (val-form-param
    :type list
    :initform nil
    :initarg :val-form-param
    :accessor var-enode-val-form-param)))

(defmethod enode-initialize-extension ((node var-enode) (ext rnode))
  (multiple-value-bind (val-form-param-text
                        val-form-param-line-count)
      (format-form (var-enode-val-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-0+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (enode-get-ynode-anchor (enode-parent node))))
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
                                :text (format nil "~S" (var-enode-name-param node))))
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
                                :text (format nil "~S" (var-enode-aux-form-param node))))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 9
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
                               :text val-form-param-text
                               :h-co val-form-param-line-count)))
      (setf (rnode-rdatas ext)
            (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nar)))))

;; TODO
(defmethod enode-get-rdata-options ((node var-enode) id-sub)
  (case id-sub
    (1 (list "var expr-label-rdata"))
    (4 (list "var attr-label-rdata NAME"))
    (5 (list "var attr-input-rdata NAME"))
    (7 (list "var attr-label-rdata AUX FORM"))
    (8 (list "var attr-input-rdata AUX FORM"))
    (9 (list "var attr-input-rdata VAL FORM"))))

(defclass each-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor each-enode-name-param)
   (keys-form-param
    :type list
    :initform nil
    :initarg :keys-form-param
    :accessor each-enode-keys-form-param)
   (vals-form-param
    :type list
    :initform nil
    :initarg :vals-form-param
    :accessor each-enode-vals-form-param)))

(defmethod enode-initialize-extension ((node each-enode) (ext rnode))
  (multiple-value-bind (vals-form-param-text
                        vals-form-param-line-count)
      (format-form (each-enode-vals-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-1+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (enode-get-ynode-anchor (enode-parent node))))
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
                                :text (format nil "~S" (each-enode-name-param node))))
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
                                :text (format nil "~S" (each-enode-keys-form-param node))))
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
      (setf (rnode-rdatas ext)
            (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nac2 nal2 nai2)))))

;; TODO
(defmethod enode-get-rdata-options ((node each-enode) id-sub)
  (case id-sub
    (1 (list "each expr-label-rdata"))
    (4 (list "each attr-label-rdata NAME"))
    (5 (list "each attr-input-rdata NAME"))
    (7 (list "each attr-label-rdata KEY(S)"))
    (8 (list "each attr-input-rdata KEY(S)"))
    (10 (list "each attr-label-rdata VALUE(S)"))
    (11 (list "each attr-input-rdata VALUE(S)"))))

(defclass iota-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor iota-enode-name-param)
   (key-param
    :type base-string
    :initarg :key-param
    :accessor iota-enode-key-param)
   (end-param
    :type integer
    :initarg :end-param
    :accessor iota-enode-end-param)))

(defmethod enode-initialize-extension ((node iota-enode) (ext rnode))
  (let* ((color mopr-def:+command-theme-expr-bg-2+)
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent (enode-get-ynode-anchor (enode-parent node))))
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
                              :text (format nil "~S" (iota-enode-name-param node))))
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
                              :text (format nil "~S" (iota-enode-key-param node))))
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
                              :text (format nil "~S" (iota-enode-end-param node)))))
    (setf (rnode-rdatas ext)
          (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nac2 nal2 nai2))))

;; TODO
(defmethod enode-get-rdata-options ((node iota-enode) id-sub)
  (case id-sub
    (1 (list "iota expr-label-rdata"))
    (4 (list "iota attr-label-rdata NAME"))
    (5 (list "iota attr-input-rdata NAME"))
    (7 (list "iota attr-label-rdata KEY"))
    (8 (list "iota attr-input-rdata KEY"))
    (10 (list "iota attr-label-rdata END"))
    (11 (list "iota attr-input-rdata END"))))

(defclass call-enode (enode)
  ((aux-form-param
    :type list
    :initarg :aux-form-param
    :accessor call-enode-aux-form-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor call-enode-body-form-param)))

(defmethod enode-initialize-extension ((node call-enode) (ext rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (call-enode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-3+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (enode-get-ynode-anchor (enode-parent node))))
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
                                :text (format nil "~S" (call-enode-aux-form-param node))))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 6
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas ext)
            (list nec nel ncc nac0 nal0 nai0 nar)))))

;; TODO
(defmethod enode-get-rdata-options ((node call-enode) id-sub)
  (case id-sub
    (1 (list "call expr-label-rdata"))
    (4 (list "call attr-label-rdata AUX"))
    (5 (list "call attr-input-rdata AUX"))
    (6 (list "call attr-input-rdata BODY"))))

(defclass prim-call-enode (call-enode)
  ())

(defclass prim-type-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-type-enode-name-param)))

(defmethod enode-initialize-extension ((node prim-type-enode) (ext rnode))
  (let* ((color mopr-def:+command-theme-expr-bg-4+)
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent (enode-get-ynode-anchor (enode-parent node))))
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
                              :text (format nil "~S" (prim-type-enode-name-param node)))))
    (setf (rnode-rdatas ext)
          (list nec nel ncc nac0 nal0 nai0))))

;; TODO
(defmethod enode-get-rdata-options ((node prim-type-enode) id-sub)
  (case id-sub
    (1 (list "prim-type expr-label-rdata"))
    (4 (list "prim-type attr-label-rdata NAME"))
    (5 (list "prim-type attr-input-rdata NAME"))))

(defclass prim-attr-enode (enode)
  ((name-param
    :type (or symbol base-string)
    :initarg :name-param
    :accessor prim-attr-enode-name-param)
   (meta-form-param
    :type list
    :initarg :meta-form-param
    :initform nil
    :accessor prim-attr-enode-meta-form-param)
   (category-param
    :type keyword
    :initarg :category-param
    :accessor prim-attr-enode-category-param)
   (type-param
    :type keyword
    :initarg :type-param
    :accessor prim-attr-enode-type-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-attr-enode-body-form-param)))

(defmethod enode-initialize-extension ((node prim-attr-enode) (ext rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (prim-attr-enode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-8+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (enode-get-ynode-anchor (enode-parent node))))
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
                                :text (format nil "~S" (prim-attr-enode-name-param node))))
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
                                :text (format nil "~S" (prim-attr-enode-meta-form-param node))))
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
                                :text (format nil "~S" (prim-attr-enode-category-param node))))
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
                                :text (format nil "~S" (prim-attr-enode-type-param node))))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 15
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas ext)
            (list nec nel ncc
                  nac0 nal0 nai0
                  nac1 nal1 nai1
                  nac2 nal2 nai2
                  nac3 nal3 nai3
                  nar)))))

;; TODO
(defmethod enode-get-rdata-options ((node prim-attr-enode) id-sub)
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

(defclass prim-rel-enode (enode)
  ((name-param
    :type (or symbol base-string)
    :initarg :name-param
    :accessor prim-rel-enode-name-param)
   (meta-form-param
    :type list
    :initarg :meta-form-param
    :initform nil
    :accessor prim-rel-enode-meta-form-param)
   (body-form-param
    :type list
    :initarg :body-form-param
    :accessor prim-rel-enode-body-form-param)))

(defmethod enode-initialize-extension ((node prim-rel-enode) (ext rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (prim-rel-enode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-8+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (enode-get-ynode-anchor (enode-parent node))))
           (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                               :id 1
                               :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                               :text "REL"
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
                                :text (format nil "~S" (prim-rel-enode-name-param node))))
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
                                :text (format nil "~S" (prim-rel-enode-meta-form-param node))))
           (nar (make-instance 'mopr-ext/repr-rdata:attr-input-rdata
                               :id 9
                               :yparent (mopr-ext/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas ext)
            (list nec nel ncc
                  nac0 nal0 nai0
                  nac1 nal1 nai1
                  nar)))))

;; TODO
(defmethod enode-get-rdata-options ((node prim-rel-enode) id-sub)
  (case id-sub
    (1 (list "prim-rel expr-label-rdata"))
    (4 (list "prim-rel attr-label-rdata NAME"))
    (5 (list "prim-rel attr-input-rdata NAME"))
    (7 (list "prim-rel attr-label-rdata META"))
    (8 (list "prim-rel attr-input-rdata META"))
    (9 (list "prim-rel attr-input-rdata BODY"))))

(defclass prim-ns-enode (enode)
  ((name-param
    :type base-string
    :initarg :name-param
    :accessor prim-ns-enode-name-param)))

(defmethod enode-initialize-extension ((node prim-ns-enode) (ext rnode))
  (let* ((color mopr-def:+command-theme-expr-bg-9+)
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent (enode-get-ynode-anchor (enode-parent node))))
         (nel (make-instance 'mopr-ext/repr-rdata:expr-label-rdata
                             :id 1
                             :yparent (mopr-ext/repr-rdata:rdata-ynode nec)
                             :text "NS"
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
                              :text (format nil "~S" (prim-ns-enode-name-param node)))))
    (setf (rnode-rdatas ext)
          (list nec nel ncc nac0 nal0 nai0))))

;; TODO
(defmethod enode-get-rdata-options ((node prim-ns-enode) id-sub)
  (case id-sub
    (1 (list "prim-ns expr-label-rdata"))
    (4 (list "prim-ns attr-label-rdata NAME"))
    (5 (list "prim-ns attr-input-rdata NAME"))))

(defmethod enode-get-ynode-anchor ((n prim-ns-enode))
  (mopr-ext/repr-rdata:rdata-ynode (caddr (enode-rdatas n))))

(defclass prim-enode (enode)
  ((path-form-param
    :type list
    :initarg :path-form-param
    :accessor prim-enode-path-form-param)))

(defmethod enode-initialize-extension ((node prim-enode) (ext rnode))
  (let* ((color mopr-def:+command-theme-expr-bg-5+)
         (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent (enode-get-ynode-anchor (enode-parent node))))
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
                              :text (format nil "~S" (prim-enode-path-form-param node)))))
    (setf (rnode-rdatas ext)
          (list nec nel ncc nac0 nal0 nai0))))

;; TODO
(defmethod enode-get-rdata-options ((node prim-enode) id-sub)
  (case id-sub
    (1 (list "prim expr-label-rdata"))
    (4 (list "prim attr-label-rdata PATH"))
    (5 (list "prim attr-input-rdata PATH"))))

(defmethod enode-get-ynode-anchor ((n prim-enode))
  (mopr-ext/repr-rdata:rdata-ynode (caddr (enode-rdatas n))))

(defclass tree-enode (enode)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor tree-enode-body-form-param)))

(defmethod enode-initialize-extension ((node tree-enode) (ext rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (tree-enode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-6+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (enode-get-ynode-anchor (enode-parent node))))
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
      (setf (rnode-rdatas ext)
            (list nec nel ncc nar)))))

;; TODO
(defmethod enode-get-rdata-options ((node tree-enode) id-sub)
  (case id-sub
    (1 (list "tree expr-label-rdata"))
    (3 (list "tree attr-input-rdata BODY"))))

(defclass meta-enode (enode)
  ((body-form-param
    :type list
    :initarg :body-form-param
    :accessor meta-enode-body-form-param)))

;; TODO : Add support for metadata handling.
(defmethod enode-initialize-extension ((node meta-enode) (ext rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (meta-enode-body-form-param node) *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-7+)
           (nec (make-instance 'mopr-ext/repr-rdata:expr-container-rdata
                               :id 0
                               :yparent (enode-get-ynode-anchor (enode-parent node))))
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
      (setf (rnode-rdatas ext)
            (list nec nel ncc nar)))))

;; TODO
(defmethod enode-get-rdata-options ((node meta-enode) id-sub)
  (case id-sub
    (1 (list "meta expr-label-rdata"))
    (3 (list "meta attr-input-rdata BODY"))))

(defclass prim-meta-enode (meta-enode)
  ())
