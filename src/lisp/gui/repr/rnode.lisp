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

   ;; RNODE API
   #:rnode
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

(defclass rnode ()
  ((rdatas
    :type list
    :initform nil
    :accessor rnode-rdatas)))

(defmethod mopr-sgt:enode-procedure-init-component-unchecked (pr (cc (eql 'rnode)))
  (mopr-gui/layout-shared:with-layout-settings
      (call-next-method)))

(defmethod mopr-sgt:enode-procedure-term-component-unchecked (pr (cc (eql 'rnode)))
  (mopr-gui/layout-shared:with-layout-settings
      (call-next-method)))

;;
;;; ENODE API
;;

(defgeneric enode-get-ynode-anchor-index (node)
  (:documentation "Get the index of ynode that should contain child ynodes."))

(defun enode-get-ynode-anchor (n &aux (rn (enode-find-component n 'rnode)))
  (mopr-gui/repr-rdata:rdata-ynode
   (elt (rnode-rdatas rn)
        (enode-get-ynode-anchor-index (bnode-find-payload n)))))

(defmethod enode-get-ynode-anchor-index ((n enode))
  (error (format nil "ENODE type ~A doesn't support children!" (class-name (class-of n)))))

(defun get-expr-rdatas (color node label)
  (let* ((nec (make-instance 'mopr-gui/repr-rdata:expr-container-rdata
                             :yparent (enode-get-ynode-anchor (enode-parent node))))
         (nel (make-instance 'mopr-gui/repr-rdata:expr-label-rdata
                             :yparent (mopr-gui/repr-rdata:rdata-ynode nec)
                             :text label
                             :bg color))
         (ncc (make-instance 'mopr-gui/repr-rdata:content-container-rdata
                             :yparent (mopr-gui/repr-rdata:rdata-ynode nec))))
    (list nec nel ncc)))

(defun get-attr-rdatas (color container label &rest input-args)
  (let* ((nac (make-instance 'mopr-gui/repr-rdata:attr-container-rdata
                             :yparent (mopr-gui/repr-rdata:rdata-ynode container)))
         (nac-ynode (mopr-gui/repr-rdata:rdata-ynode nac))
         (nal (make-instance 'mopr-gui/repr-rdata:attr-label-rdata
                             :yparent nac-ynode
                             :text label
                             :bg color))
         (nai (apply #'make-instance
                     'mopr-gui/repr-rdata:attr-input-rdata
                     :yparent nac-ynode
                     input-args)))
    (list nac nal nai)))

;;
;;; ROOT-CONTAINER API
;;

(defmethod enode-init-component ((payload root-container) node (component rnode))
  (let* ((nrc (make-instance 'mopr-gui/repr-rdata:root-container-rdata)))
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
           (na0 (get-attr-rdatas color ncc "NAME"
                                 :text (format nil "~S" (var-directive-name-param payload))))
           (na1 (get-attr-rdatas color ncc "AUX"
                                 :text (format nil "~S" (var-directive-aux-form-param payload))))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text val-form-param-text
                               :h-co val-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 na1 (list nar))))))

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
           (na0 (get-attr-rdatas color ncc "NAME"
                                 :text (format nil "~S" (each-directive-name-param payload))))
           (na1 (get-attr-rdatas color ncc "KEY(S)"
                                 :text (format nil "~S" (each-directive-keys-form-param payload))))
           (na2 (get-attr-rdatas color ncc "VALUE(S)"
                                 :text vals-form-param-text
                                 :h-co vals-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 na1 na2)))))

;;
;;; IOTA-DIRECTIVE API
;;

(defmethod enode-init-component ((payload iota-directive) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-2+)
         (ne (get-expr-rdatas color node "IOTA"))
         (ncc (third ne))
         (na0 (get-attr-rdatas color ncc "NAME"
                               :text (format nil "~S" (iota-directive-name-param payload))))
         (na1 (get-attr-rdatas color ncc "KEY"
                               :text (format nil "~S" (iota-directive-key-param payload))))
         (na2 (get-attr-rdatas color ncc "END"
                               :text (format nil "~S" (iota-directive-end-param payload))))
         (na3 (get-attr-rdatas color ncc "START"
                               :text (format nil "~S" (iota-directive-start-param payload))))
         (na4 (get-attr-rdatas color ncc "STEP"
                               :text (format nil "~S" (iota-directive-step-param payload)))))
    (setf (rnode-rdatas component)
          (nconc ne na0 na1 na2 na3 na4))))

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
           (na0 (get-attr-rdatas color ncc "AUX"
                                 :text (format nil "~S" (call-directive-aux-form-param payload))))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 (list nar))))))

;;
;;; PRIM-TYPE-STATEMENT API
;;

(defmethod enode-init-component ((payload prim-type-statement) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-4+)
         (ne (get-expr-rdatas color node "TYPE"))
         (ncc (third ne))
         (na0 (get-attr-rdatas color ncc "NAME"
                               :text (format nil "~S" (prim-type-statement-name-param payload)))))
    (setf (rnode-rdatas component)
          (nconc ne na0))))

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
           (na0 (get-attr-rdatas color ncc "NAME"
                                 :text (format nil "~S" (prim-attr-statement-name-param payload))))
           (na1 (get-attr-rdatas color ncc "META"
                                 :text (format nil "~S" (prim-attr-statement-meta-form-param payload))))
           (na2 (get-attr-rdatas color ncc "CATEGORY"
                                 :text (format nil "~S" (prim-attr-statement-category-param payload))))
           (na3 (get-attr-rdatas color ncc "TYPE"
                                 :text (format nil "~S" (prim-attr-statement-type-param payload))))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 na1 na2 na3 (list nar))))))

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
           (na0 (get-attr-rdatas color ncc "NAME"
                                 :text (format nil "~S" (prim-rel-statement-name-param payload))))
           (na1 (get-attr-rdatas color ncc "META"
                                 :text (format nil "~S" (prim-rel-statement-meta-form-param payload))))
           (nar (make-instance 'mopr-gui/repr-rdata:attr-input-rdata
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne na0 na1 (list nar))))))

;;
;;; PRIM-NS-CONTAINER API
;;

(defmethod enode-init-component ((payload prim-ns-container) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-9+)
         (ne (get-expr-rdatas color node "NS"))
         (ncc (third ne))
         (na0 (get-attr-rdatas color ncc "NAME"
                               :text (format nil "~S" (prim-ns-container-name-param payload)))))
    (setf (rnode-rdatas component)
          (nconc ne na0))))

(defmethod enode-get-ynode-anchor-index ((payload prim-ns-container)) 2)

;;
;;; PRIM-STATEMENT API
;;

(defmethod enode-init-component ((payload prim-statement) node (component rnode))
  (let* ((color mopr-gui/repr-def:+command-theme-expr-bg-5+)
         (ne (get-expr-rdatas color node "PRIM"))
         (ncc (third ne))
         (na0 (get-attr-rdatas color ncc "PATH"
                               :text (format nil "~S" (prim-statement-path-form-param payload)))))
    (setf (rnode-rdatas component)
          (nconc ne na0))))

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
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne (list nar))))))

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
                               :yparent (mopr-gui/repr-rdata:rdata-ynode ncc)
                               :text body-form-param-text
                               :h-co body-form-param-line-count)))
      (setf (rnode-rdatas component)
            (nconc ne (list nar))))))
