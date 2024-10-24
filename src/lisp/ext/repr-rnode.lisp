;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr-rnode
  (:import-from :mopr)
  (:import-from :mopr-ext/repr-shared
                #:multiple-set-c-ref)
  (:import-from :mopr-ext/repr-rdata)
  (:use :mopr-ext/enode)
  (:use :cl)
  (:export

   ;; Generic APIs
   #:enode-get-rdata-options
   #:find-enode-by-rnode-id
   #:populate-command-from-rnode

   ;; RNODE API
   #:rnode
   #:rnode-id
   #:rnode-rdatas

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

;;
;;; ENODE API
;;

(defgeneric enode-get-ynode-anchor-index (node)
  (:documentation "Get the index of ynode that should contain child ynodes."))

(defun enode-get-ynode-anchor (n &aux (rn (enode-find-extension n 'rnode)))
  (mopr-ext/repr-rdata:rdata-ynode
   (elt (rnode-rdatas rn)
        (enode-get-ynode-anchor-index n))))

(defgeneric enode-get-rdata-options (node id-sub)
  (:documentation "Get the options available for the selected rdata of given node."))

(defmethod enode-get-ynode-anchor-index ((n enode))
  (error (format nil "ENODE type ~A doesn't support children!" (class-name (class-of n)))))

(defmethod enode-get-rdata-options ((node enode) id-sub)
  nil)

(defun find-enode-by-rnode-id (n id &aux (rn (enode-find-extension n 'rnode)))
  (if (eql (rnode-id rn) id) n
      (loop for c across (enode-children n) for x = (find-enode-by-rnode-id c id) if x return x)))

(defun populate-command-from-rnode (rn c)
  (multiple-set-c-ref c (mopr-def:combined-command :base)
                      :id (rnode-id rn)))

;;
;;; ROOT-ENODE API
;;

(defmethod enode-initialize-extension ((node root-enode) (ext rnode))
  (let* ((nrc (make-instance 'mopr-ext/repr-rdata:root-container-rdata
                             :id 0)))
    (setf (rnode-rdatas ext)
          (list nrc))))

(defmethod enode-get-ynode-anchor-index ((n root-enode)) 0)

;;
;;; VAR-ENODE API
;;

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

;;
;;; EACH-ENODE API
;;

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

;;
;;; IOTA-ENODE API
;;

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

;;
;;; CALL-ENODE API
;;

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

;;
;;; PRIM-TYPE-ENODE API
;;

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

;;
;;; PRIM-ATTR-ENODE API
;;

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

;;
;;; PRIM-REL-ENODE API
;;

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

;;
;;; PRIM-NS-ENODE API
;;

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

(defmethod enode-get-ynode-anchor-index ((n prim-ns-enode)) 2)

;;
;;; PRIM-ENODE API
;;

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

(defmethod enode-get-ynode-anchor-index ((n prim-enode)) 2)

;;
;;; TREE-ENODE API
;;

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

;;
;;; META-ENODE API
;;

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
