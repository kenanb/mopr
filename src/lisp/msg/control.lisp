;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-msg/ctrl

  (:use :mopr-sgt)
  (:use :cl)
  (:export

   ;; Generic APIs
   #:payload-get-options
   #:find-enode-by-id

   ;; NODE-IDENTIFIER API
   #:node-identifier
   #:node-identifier-id

   ))

(in-package :mopr-msg/ctrl)

(defvar *node-identifier-id-counter*)

(defclass node-identifier ()
  ((id
    :type (unsigned-byte 32)
    ;; Zero value is reserved for "no selection", so INITFORM will INCF.
    :initform (incf *node-identifier-id-counter*)
    :reader node-identifier-id)))

(defmethod enode-init-component ((payload payload) node (component node-identifier))
  nil)

(defmethod enode-term-component ((payload payload) node (component node-identifier))
  nil)

(defmethod mopr-sgt:enode-procedure-create-component-unchecked (pr (cc (eql 'node-identifier)))
  ;; Zero value is reserved for "no selection".
  (let ((*node-identifier-id-counter* 0))
    (call-next-method)))

(defun find-enode-by-id (n id &aux (rn (enode-find-component n 'node-identifier)))
  (if (eql (node-identifier-id rn) id) n
      (loop for c across (enode-children n) for x = (find-enode-by-id c id) if x return x)))

(defgeneric payload-get-options (payload id-sub)
  (:documentation "Get the options available for the selected entity of given node.")
  (:method (payload id-sub)
    nil))

;;
;;; ROOT-CONTAINER API
;;

;;
;;; GROUP-CONTAINER API
;;

(defconstant +options-group-container+
  '(("group expr-label")))

(defmethod payload-get-options ((payload group-container) id-sub)
  (nth id-sub +options-group-container+))

;;
;;; VAR-DIRECTIVE API
;;

(defconstant +options-var-directive+
  '(("var expr-label")
    ("var attr-label NAME")
    ("var attr-input NAME")
    ("var attr-label AUX FORM")
    ("var attr-input AUX FORM")
    ("var attr-input VAL FORM")))

(defmethod payload-get-options ((payload var-directive) id-sub)
  (nth id-sub +options-var-directive+))

;;
;;; EACH-DIRECTIVE API
;;

(defconstant +options-each-directive+
  '(("each expr-label")
    ("each attr-label NAME")
    ("each attr-input NAME")
    ("each attr-label KEY(S)")
    ("each attr-input KEY(S)")
    ("each attr-label VALUE(S)")
    ("each attr-input VALUE(S)")))

(defmethod payload-get-options ((payload each-directive) id-sub)
  (nth id-sub +options-each-directive+))

;;
;;; IOTA-DIRECTIVE API
;;

(defconstant +options-iota-directive+
  '(("iota expr-label")
    ("iota attr-label NAME")
    ("iota attr-input NAME")
    ("iota attr-label KEY")
    ("iota attr-input KEY")
    ("iota attr-label END")
    ("iota attr-input END")
    ("iota attr-label START")
    ("iota attr-input START")
    ("iota attr-label STEP")
    ("iota attr-input STEP")))

(defmethod payload-get-options ((payload iota-directive) id-sub)
  (nth id-sub +options-iota-directive+))

;;
;;; CALL-DIRECTIVE API
;;

(defconstant +options-call-directive+
  '(("call expr-label")
    ("call attr-label AUX")
    ("call attr-input AUX")
    ("call attr-input BODY")))

(defmethod payload-get-options ((payload call-directive) id-sub)
  (nth id-sub +options-call-directive+))

;;
;;; PRIM-TYPE-STATEMENT API
;;

(defconstant +options-prim-type-statement+
  '(("prim-type expr-label")
    ("prim-type attr-label NAME")
    ("prim-type attr-input NAME")))

(defmethod payload-get-options ((payload prim-type-statement) id-sub)
  (nth id-sub +options-prim-type-statement+))

;;
;;; PRIM-ATTR-STATEMENT API
;;

(defconstant +options-prim-attr-statement+
  '(("prim-attr expr-label")
    ("prim-attr attr-label NAME")
    ("prim-attr attr-input NAME")
    ("prim-attr attr-label META")
    ("prim-attr attr-input META")
    ("prim-attr attr-label CATEGORY")
    ("prim-attr attr-input CATEGORY")
    ("prim-attr attr-label TYPE")
    ("prim-attr attr-input TYPE")
    ("prim-attr attr-input BODY")))

(defmethod payload-get-options ((payload prim-attr-statement) id-sub)
  (nth id-sub +options-prim-attr-statement+))

;;
;;; PRIM-REL-STATEMENT API
;;

(defconstant +options-prim-rel-statement+
  '(("prim-rel expr-label")
    ("prim-rel attr-label NAME")
    ("prim-rel attr-input NAME")
    ("prim-rel attr-label META")
    ("prim-rel attr-input META")
    ("prim-rel attr-input BODY")))

(defmethod payload-get-options ((payload prim-rel-statement) id-sub)
  (nth id-sub +options-prim-rel-statement+))

;;
;;; PRIM-NS-CONTAINER API
;;

(defconstant +options-prim-ns-container+
  '(("prim-ns expr-label")
    ("prim-ns attr-label NAME")
    ("prim-ns attr-input NAME")))

(defmethod payload-get-options ((payload prim-ns-container) id-sub)
  (nth id-sub +options-prim-ns-container+))

;;
;;; PRIM-STATEMENT API
;;

(defconstant +options-prim-statement+
  '(("prim expr-label")
    ("prim attr-label PATH")
    ("prim attr-input PATH")))

(defmethod payload-get-options ((payload prim-statement) id-sub)
  (nth id-sub +options-prim-statement+))

;;
;;; TREE-STATEMENT API
;;

(defconstant +options-tree-statement+
  '(("tree expr-label")
    ("tree attr-input BODY")))

(defmethod payload-get-options ((payload tree-statement) id-sub)
  (nth id-sub +options-tree-statement+))

;;
;;; META-STATEMENT API
;;

(defconstant +options-meta-statement+
  '(("meta expr-label")
    ("meta attr-input BODY")))

(defmethod payload-get-options ((payload meta-statement) id-sub)
  (nth id-sub +options-meta-statement+))
