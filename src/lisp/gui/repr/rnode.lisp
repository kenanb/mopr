;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-gui/repr-rnode
  (:import-from :mopr-gui/repr-shared
                #:multiple-set-c-ref)
  (:import-from :mopr-gui/repr-rdata)
  (:use :mopr-sgt)
  (:use :cl)
  (:export

   ;; Generic APIs
   #:payload-get-rdata-options
   #:find-enode-by-rnode-id
   #:populate-command-from-rnode

   ;; RNODE API
   #:rnode
   #:rnode-id
   #:rnode-rdatas

   ))

(in-package :mopr-gui/repr-rnode)

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

(defun enode-get-ynode-anchor (n &aux (rn (enode-find-component n 'rnode)))
  (mopr-gui/repr-rdata:rdata-ynode
   (elt (rnode-rdatas rn)
        (enode-get-ynode-anchor-index (bnode-find-payload n)))))

(defgeneric payload-get-rdata-options (payload id-sub)
  (:documentation "Get the options available for the selected rdata of given node."))

(defmethod enode-get-ynode-anchor-index ((n enode))
  (error (format nil "ENODE type ~A doesn't support children!" (class-name (class-of n)))))

(defmethod payload-get-rdata-options ((node enode) id-sub)
  nil)

(defun find-enode-by-rnode-id (n id &aux (rn (enode-find-component n 'rnode)))
  (if (eql (rnode-id rn) id) n
      (loop for c across (enode-children n) for x = (find-enode-by-rnode-id c id) if x return x)))

(defun populate-command-from-rnode (rn c)
  (multiple-set-c-ref c (mopr-gui/repr-def:combined-command :base)
                      :id (rnode-id rn)))

(defun get-expr-rdatas (color node label)
  (let* ((nec (make-instance 'mopr-gui/repr-rdata:expr-container-rdata
                             :id 0
                             :yparent (enode-get-ynode-anchor (enode-parent node))))
         (nel (make-instance 'mopr-gui/repr-rdata:expr-label-rdata
                             :id 1
                             :yparent (mopr-gui/repr-rdata:rdata-ynode nec)
                             :text label
                             :bg color))
         (ncc (make-instance 'mopr-gui/repr-rdata:content-container-rdata
                             :id 2
                             :yparent (mopr-gui/repr-rdata:rdata-ynode nec))))
    (list nec nel ncc)))

(defun get-attr-rdatas (color container id-offset label &rest input-args)
  (let* ((nac (make-instance 'mopr-gui/repr-rdata:attr-container-rdata
                             :id (+ id-offset 0)
                             :yparent (mopr-gui/repr-rdata:rdata-ynode container)))
         (nac-ynode (mopr-gui/repr-rdata:rdata-ynode nac))
         (nal (make-instance 'mopr-gui/repr-rdata:attr-label-rdata
                             :id (+ id-offset 1)
                             :yparent nac-ynode
                             :text label
                             :bg color))
         (nai (apply #'make-instance
                     'mopr-gui/repr-rdata:attr-input-rdata
                     :id (+ id-offset 2)
                     :yparent nac-ynode
                     input-args)))
    (list nac nal nai)))

;;
;;; ROOT-CONTAINER API
;;

(defmethod enode-init-component ((payload root-container) node (component rnode))
  (let* ((nrc (make-instance 'mopr-gui/repr-rdata:root-container-rdata
                             :id 0)))
    (setf (rnode-rdatas component)
          (list nrc))))

(defmethod enode-get-ynode-anchor-index ((payload root-container)) 0)

(defmethod enode-term-component ((payload payload) node (component rnode))
  nil)

(defmethod enode-term-component ((payload root-container) node (component rnode))
  (let* ((rn (mopr-sgt:enode-find-component node 'mopr-gui/repr-rnode:rnode))
         (rd (mopr-gui/repr-rnode:rnode-rdatas rn))
         (yn (mopr-gui/repr-rdata:rdata-ynode (car rd))))
    (mopr-gui/yoga-fun:node-free-recursive yn)))

;;
;;; GROUP-CONTAINER API
;;

(defmethod enode-init-component ((payload group-container) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-9+)
         (ne (get-expr-rdatas color node "GROUP")))
    (setf (rnode-rdatas component)
          (nconc ne))))

(defmethod enode-get-ynode-anchor-index ((payload group-container)) 2)

;; TODO
(defmethod payload-get-rdata-options ((payload group-container) id-sub)
  (case id-sub
    (1 (list "group expr-label-rdata"))))

;;
;;; VAR-DIRECTIVE API
;;

(defmethod enode-init-component ((payload var-directive) node (component rnode))
  (multiple-value-bind (val-form-param-text
                        val-form-param-line-count)
      (format-form (var-directive-val-form-param payload) *fill-column*)
    (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-0+)
           (ne (get-expr-rdatas color node "VAR"))
           (ncc (third ne))
           (na0 (get-attr-rdatas color ncc 3 "NAME"
                                 :text (format nil "~S" (var-directive-name-param payload))))
           (na1 (get-attr-rdatas color ncc 6 "AUX"
                                 :text (format nil "~S" (var-directive-aux-form-param payload))))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :id 9
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text val-form-param-text
                               :h-co val-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 na1 (list nar))))))

;; TODO
(defmethod payload-get-rdata-options ((payload var-directive) id-sub)
  (case id-sub
    (1 (list "var expr-label-rdata"))
    (4 (list "var attr-label-rdata NAME"))
    (5 (list "var attr-input-rdata NAME"))
    (7 (list "var attr-label-rdata AUX FORM"))
    (8 (list "var attr-input-rdata AUX FORM"))
    (9 (list "var attr-input-rdata VAL FORM"))))

;;
;;; EACH-DIRECTIVE API
;;

(defmethod enode-init-component ((payload each-directive) node (component rnode))
  (multiple-value-bind (vals-form-param-text
                        vals-form-param-line-count)
      (format-form (each-directive-vals-form-param payload) *fill-column*)
    (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-1+)
           (ne (get-expr-rdatas color node "EACH"))
           (ncc (third ne))
           (na0 (get-attr-rdatas color ncc 3 "NAME"
                                 :text (format nil "~S" (each-directive-name-param payload))))
           (na1 (get-attr-rdatas color ncc 6 "KEY(S)"
                                 :text (format nil "~S" (each-directive-keys-form-param payload))))
           (na2 (get-attr-rdatas color ncc 9 "VALUE(S)"
                                 :text vals-form-param-text
                                 :h-co vals-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 na1 na2)))))

;; TODO
(defmethod payload-get-rdata-options ((payload each-directive) id-sub)
  (case id-sub
    (1 (list "each expr-label-rdata"))
    (4 (list "each attr-label-rdata NAME"))
    (5 (list "each attr-input-rdata NAME"))
    (7 (list "each attr-label-rdata KEY(S)"))
    (8 (list "each attr-input-rdata KEY(S)"))
    (10 (list "each attr-label-rdata VALUE(S)"))
    (11 (list "each attr-input-rdata VALUE(S)"))))

;;
;;; IOTA-DIRECTIVE API
;;

(defmethod enode-init-component ((payload iota-directive) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-2+)
         (ne (get-expr-rdatas color node "IOTA"))
         (ncc (third ne))
         (na0 (get-attr-rdatas color ncc 3 "NAME"
                               :text (format nil "~S" (iota-directive-name-param payload))))
         (na1 (get-attr-rdatas color ncc 6 "KEY"
                               :text (format nil "~S" (iota-directive-key-param payload))))
         (na2 (get-attr-rdatas color ncc 9 "END"
                               :text (format nil "~S" (iota-directive-end-param payload))))
         (na3 (get-attr-rdatas color ncc 12 "START"
                               :text (format nil "~S" (iota-directive-start-param payload))))
         (na4 (get-attr-rdatas color ncc 15 "STEP"
                               :text (format nil "~S" (iota-directive-step-param payload)))))
    (setf (rnode-rdatas component)
          (nconc ne na0 na1 na2 na3 na4))))

;; TODO
(defmethod payload-get-rdata-options ((payload iota-directive) id-sub)
  (case id-sub
    (1 (list "iota expr-label-rdata"))
    (4 (list "iota attr-label-rdata NAME"))
    (5 (list "iota attr-input-rdata NAME"))
    (7 (list "iota attr-label-rdata KEY"))
    (8 (list "iota attr-input-rdata KEY"))
    (10 (list "iota attr-label-rdata END"))
    (11 (list "iota attr-input-rdata END"))
    (13 (list "iota attr-label-rdata START"))
    (14 (list "iota attr-input-rdata START"))
    (16 (list "iota attr-label-rdata STEP"))
    (17 (list "iota attr-input-rdata STEP"))))

;;
;;; CALL-DIRECTIVE API
;;

(defmethod enode-init-component ((payload call-directive) node (component rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (call-directive-body-form-param payload) *fill-column*)
    (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-3+)
           (ne (get-expr-rdatas color node "CALL"))
           (ncc (third ne))
           (na0 (get-attr-rdatas color ncc 3 "AUX"
                                 :text (format nil "~S" (call-directive-aux-form-param payload))))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :id 6
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 (list nar))))))

;; TODO
(defmethod payload-get-rdata-options ((payload call-directive) id-sub)
  (case id-sub
    (1 (list "call expr-label-rdata"))
    (4 (list "call attr-label-rdata AUX"))
    (5 (list "call attr-input-rdata AUX"))
    (6 (list "call attr-input-rdata BODY"))))

;;
;;; PRIM-TYPE-STATEMENT API
;;

(defmethod enode-init-component ((payload prim-type-statement) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-4+)
         (ne (get-expr-rdatas color node "TYPE"))
         (ncc (third ne))
         (na0 (get-attr-rdatas color ncc 3 "NAME"
                               :text (format nil "~S" (prim-type-statement-name-param payload)))))
    (setf (rnode-rdatas component)
          (nconc ne na0))))

;; TODO
(defmethod payload-get-rdata-options ((payload prim-type-statement) id-sub)
  (case id-sub
    (1 (list "prim-type expr-label-rdata"))
    (4 (list "prim-type attr-label-rdata NAME"))
    (5 (list "prim-type attr-input-rdata NAME"))))

;;
;;; PRIM-ATTR-STATEMENT API
;;

(defmethod enode-init-component ((payload prim-attr-statement) node (component rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (prim-attr-statement-body-form-param payload) *fill-column*)
    (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-8+)
           (ne (get-expr-rdatas color node "ATTR"))
           (ncc (third ne))
           (na0 (get-attr-rdatas color ncc 3 "NAME"
                                 :text (format nil "~S" (prim-attr-statement-name-param payload))))
           (na1 (get-attr-rdatas color ncc 6 "META"
                                 :text (format nil "~S" (prim-attr-statement-meta-form-param payload))))
           (na2 (get-attr-rdatas color ncc 9 "CATEGORY"
                                 :text (format nil "~S" (prim-attr-statement-category-param payload))))
           (na3 (get-attr-rdatas color ncc 12 "TYPE"
                                 :text (format nil "~S" (prim-attr-statement-type-param payload))))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :id 15
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 na1 na2 na3 (list nar))))))

;; TODO
(defmethod payload-get-rdata-options ((payload prim-attr-statement) id-sub)
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
;;; PRIM-REL-STATEMENT API
;;

(defmethod enode-init-component ((payload prim-rel-statement) node (component rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (prim-rel-statement-body-form-param payload) *fill-column*)
    (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-8+)
           (ne (get-expr-rdatas color node "REL"))
           (ncc (third ne))
           (na0 (get-attr-rdatas color ncc 3 "NAME"
                                 :text (format nil "~S" (prim-rel-statement-name-param payload))))
           (na1 (get-attr-rdatas color ncc 6 "META"
                                 :text (format nil "~S" (prim-rel-statement-meta-form-param payload))))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :id 9
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 na1 (list nar))))))

;; TODO
(defmethod payload-get-rdata-options ((payload prim-rel-statement) id-sub)
  (case id-sub
    (1 (list "prim-rel expr-label-rdata"))
    (4 (list "prim-rel attr-label-rdata NAME"))
    (5 (list "prim-rel attr-input-rdata NAME"))
    (7 (list "prim-rel attr-label-rdata META"))
    (8 (list "prim-rel attr-input-rdata META"))
    (9 (list "prim-rel attr-input-rdata BODY"))))

;;
;;; PRIM-NS-CONTAINER API
;;

(defmethod enode-init-component ((payload prim-ns-container) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-9+)
         (ne (get-expr-rdatas color node "NS"))
         (ncc (third ne))
         (na0 (get-attr-rdatas color ncc 3 "NAME"
                               :text (format nil "~S" (prim-ns-container-name-param payload)))))
    (setf (rnode-rdatas component)
          (nconc ne na0))))

;; TODO
(defmethod payload-get-rdata-options ((payload prim-ns-container) id-sub)
  (case id-sub
    (1 (list "prim-ns expr-label-rdata"))
    (4 (list "prim-ns attr-label-rdata NAME"))
    (5 (list "prim-ns attr-input-rdata NAME"))))

(defmethod enode-get-ynode-anchor-index ((payload prim-ns-container)) 2)

;;
;;; PRIM-STATEMENT API
;;

(defmethod enode-init-component ((payload prim-statement) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-5+)
         (ne (get-expr-rdatas color node "PRIM"))
         (ncc (third ne))
         (na0 (get-attr-rdatas color ncc 3 "PATH"
                               :text (format nil "~S" (prim-statement-path-form-param payload)))))
    (setf (rnode-rdatas component)
          (nconc ne na0))))

;; TODO
(defmethod payload-get-rdata-options ((payload prim-statement) id-sub)
  (case id-sub
    (1 (list "prim expr-label-rdata"))
    (4 (list "prim attr-label-rdata PATH"))
    (5 (list "prim attr-input-rdata PATH"))))

(defmethod enode-get-ynode-anchor-index ((payload prim-statement)) 2)

;;
;;; TREE-STATEMENT API
;;

(defmethod enode-init-component ((payload tree-statement) node (component rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (tree-statement-body-form-param payload) *fill-column*)
    (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-6+)
           (ne (get-expr-rdatas color node "TREE"))
           (ncc (third ne))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :id 3
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne (list nar))))))

;; TODO
(defmethod payload-get-rdata-options ((payload tree-statement) id-sub)
  (case id-sub
    (1 (list "tree expr-label-rdata"))
    (3 (list "tree attr-input-rdata BODY"))))

;;
;;; META-STATEMENT API
;;

;; TODO : Add support for metadata handling.
(defmethod enode-init-component ((payload meta-statement) node (component rnode))
  (multiple-value-bind (body-form-param-text
                        body-form-param-line-count)
      (format-form (meta-statement-body-form-param payload) *fill-column*)
    (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-7+)
           (ne (get-expr-rdatas color node "META"))
           (ncc (third ne))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :id 3
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne (list nar))))))

;; TODO
(defmethod payload-get-rdata-options ((payload meta-statement) id-sub)
  (case id-sub
    (1 (list "meta expr-label-rdata"))
    (3 (list "meta attr-input-rdata BODY"))))
