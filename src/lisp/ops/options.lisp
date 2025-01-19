;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :mopr-ops)

(defgeneric payload-get-options (payload id-sub)
  (:documentation "Get the options available for the selected part of given node.")
  (:method (payload id-sub)
    nil))

(defun enode-procedure-calculate-command-options (pr id-node id-sub)
  (when (zerop id-sub) (error "Zero id-sub passed to enode-procedure-calculate-command-options!"))
  (mopr-sgt:with-bound-procedure-accessors ((root mopr-sgt:procedure-root)) pr
    (let* ((n (find-enode-by-id root id-node))
           (payload-opts (payload-get-options (mopr-sgt:bnode-find-payload n) (1- id-sub))))
      (cons `(("id-node" ,id-node) ("id-sub" ,id-sub))
            (loop for opt in payload-opts collect `(("name" ,opt)))))))

(defun enode-procedure-apply-command-option (pr id-node id-sub id-opt)
  (when (zerop id-sub) (error "Zero id-sub passed to enode-procedure-apply-command-option!"))
  (when (zerop id-opt) (error "Zero id-opt passed to enode-procedure-apply-command-option!"))
  (mopr-sgt:with-bound-procedure-accessors ((root mopr-sgt:procedure-root)) pr
    (let* ((n (find-enode-by-id root id-node))
           (opts (payload-get-options (mopr-sgt:bnode-find-payload n) (1- id-sub)))
           (idx (1- id-opt)))
      (format t "APPLIED OPTION: ~A~%" (nth idx opts)))))

;;
;;; ROOT-CONTAINER API
;;

;;
;;; GROUP-CONTAINER API
;;

(defconstant +options-group-container+
  '(("group expr-label")))

(defmethod payload-get-options ((payload mopr-sgt:group-container) id-sub)
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

(defmethod payload-get-options ((payload mopr-sgt:var-directive) id-sub)
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

(defmethod payload-get-options ((payload mopr-sgt:each-directive) id-sub)
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

(defmethod payload-get-options ((payload mopr-sgt:iota-directive) id-sub)
  (nth id-sub +options-iota-directive+))

;;
;;; CALL-DIRECTIVE API
;;

(defconstant +options-call-directive+
  '(("call expr-label")
    ("call attr-label AUX")
    ("call attr-input AUX")
    ("call attr-input BODY")))

(defmethod payload-get-options ((payload mopr-sgt:call-directive) id-sub)
  (nth id-sub +options-call-directive+))

;;
;;; PRIM-TYPE-STATEMENT API
;;

(defconstant +options-prim-type-statement+
  '(("prim-type expr-label")
    ("prim-type attr-label NAME")
    ("prim-type attr-input NAME")))

(defmethod payload-get-options ((payload mopr-sgt:prim-type-statement) id-sub)
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

(defmethod payload-get-options ((payload mopr-sgt:prim-attr-statement) id-sub)
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

(defmethod payload-get-options ((payload mopr-sgt:prim-rel-statement) id-sub)
  (nth id-sub +options-prim-rel-statement+))

;;
;;; PRIM-NS-CONTAINER API
;;

(defconstant +options-prim-ns-container+
  '(("prim-ns expr-label")
    ("prim-ns attr-label NAME")
    ("prim-ns attr-input NAME")))

(defmethod payload-get-options ((payload mopr-sgt:prim-ns-container) id-sub)
  (nth id-sub +options-prim-ns-container+))

;;
;;; PRIM-STATEMENT API
;;

(defconstant +options-prim-statement+
  '(("prim expr-label")
    ("prim attr-label PATH")
    ("prim attr-input PATH")))

(defmethod payload-get-options ((payload mopr-sgt:prim-statement) id-sub)
  (nth id-sub +options-prim-statement+))

;;
;;; TREE-STATEMENT API
;;

(defconstant +options-tree-statement+
  '(("tree expr-label")
    ("tree attr-input BODY")))

(defmethod payload-get-options ((payload mopr-sgt:tree-statement) id-sub)
  (nth id-sub +options-tree-statement+))

;;
;;; META-STATEMENT API
;;

(defconstant +options-meta-statement+
  '(("meta expr-label")
    ("meta attr-input BODY")))

(defmethod payload-get-options ((payload mopr-sgt:meta-statement) id-sub)
  (nth id-sub +options-meta-statement+))
