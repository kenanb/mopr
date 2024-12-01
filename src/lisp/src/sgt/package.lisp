;;;; package.lisp

(defpackage :mopr-sgt
  (:use #:cl)
  (:export

   ;; PAYLOAD CLASSES
   #:payload
   #:container
   #:directive
   #:statement

   ;; ROOT-CONTAINER
   #:make-root-container
   #:copy-root-container
   #:root-container-p
   #:root-container

   ;; GROUP-CONTAINER
   #:make-group-container
   #:copy-group-container
   #:group-container-p
   #:group-container

   ;; VAR-DIRECTIVE
   #:make-var-directive
   #:copy-var-directive
   #:var-directive-p
   #:var-directive
   #:var-directive-name-param
   #:var-directive-aux-form-param
   #:var-directive-val-form-param

   ;; EACH-DIRECTIVE
   #:make-each-directive
   #:copy-each-directive
   #:each-directive-p
   #:each-directive
   #:each-directive-name-param
   #:each-directive-keys-form-param
   #:each-directive-vals-form-param

   ;; IOTA-DIRECTIVE
   #:make-iota-directive
   #:copy-iota-directive
   #:iota-directive-p
   #:iota-directive
   #:iota-directive-name-param
   #:iota-directive-key-param
   #:iota-directive-end-param
   #:iota-directive-start-param
   #:iota-directive-step-param

   ;; CALL-DIRECTIVE
   #:make-call-directive
   #:copy-call-directive
   #:call-directive-p
   #:call-directive
   #:call-directive-aux-form-param
   #:call-directive-body-form-param

   ;; PRIM-CALL-DIRECTIVE
   #:make-prim-call-directive
   #:copy-prim-call-directive
   #:prim-call-directive-p
   #:prim-call-directive

   ;; PRIM-SCHEMA-PROP-STATEMENT
   #:make-prim-schema-prop-statement
   #:copy-prim-schema-prop-statement
   #:prim-schema-prop-statement-p
   #:prim-schema-prop-statement
   #:prim-schema-prop-statement-info-param
   #:prim-schema-prop-statement-body-form-param

   ;; PRIM-TYPE-STATEMENT
   #:make-prim-type-statement
   #:copy-prim-type-statement
   #:prim-type-statement-p
   #:prim-type-statement
   #:prim-type-statement-name-param

   ;; PRIM-NS-CONTAINER
   #:make-prim-ns-container
   #:copy-prim-ns-container
   #:prim-ns-container-p
   #:prim-ns-container
   #:prim-ns-container-name-param

   ;; PRIM-ATTR-STATEMENT
   #:make-prim-attr-statement
   #:copy-prim-attr-statement
   #:prim-attr-statement-p
   #:prim-attr-statement
   #:prim-attr-statement-name-param
   #:prim-attr-statement-meta-form-param
   #:prim-attr-statement-category-param
   #:prim-attr-statement-type-param
   #:prim-attr-statement-body-form-param

   ;; PRIM-REL-STATEMENT
   #:make-prim-rel-statement
   #:copy-prim-rel-statement
   #:prim-rel-statement-p
   #:prim-rel-statement
   #:prim-rel-statement-name-param
   #:prim-rel-statement-meta-form-param
   #:prim-rel-statement-body-form-param

   ;; PRIM-META-STATEMENT
   #:make-prim-meta-statement
   #:copy-prim-meta-statement
   #:prim-meta-statement-p
   #:prim-meta-statement

   ;; PRIM-STATEMENT
   #:make-prim-statement
   #:copy-prim-statement
   #:prim-statement-p
   #:prim-statement
   #:prim-statement-path-form-param

   ;; TREE-STATEMENT
   #:make-tree-statement
   #:copy-tree-statement
   #:tree-statement-p
   #:tree-statement
   #:tree-statement-body-form-param

   ;; META-STATEMENT
   #:make-meta-statement
   #:copy-meta-statement
   #:meta-statement-p
   #:meta-statement
   #:meta-statement-body-form-param

   ;; HEADER API
   #:make-header
   #:header-p
   #:header

   ;; CORE NODE API
   #:make-cnode
   #:cnode-p
   #:cnode
   #:cnode-payload
   #:cnode-children
   #:cnode-debug-print
   #:cnode-from-node-recursive

   ;; CNODE-SERIALIZE
   #:deserialize
   #:cnode-serialize
   #:read-from-usds-file

   ;; CNODE-CALLABLES
   #:make-prop
   #:make-group

   ;; EXTENDED NODE API
   #:make-enode
   #:enode-p
   #:enode
   #:enode-payload
   #:enode-children
   #:enode-parent
   #:enode-components
   #:enode-from-node-recursive
   #:enode-find-component
   #:enode-add-components-recursive
   #:enode-initialize-component
   #:enode-initialize-components-recursive

   ;; PROCEDURE API
   #:make-procedure
   #:make-cnode-procedure
   #:make-enode-procedure
   #:make-cnode-procedure-from-usds-file
   #:procedure-p
   #:procedure
   #:procedure-header
   #:procedure-root
   #:procedure-apply-to-layer

   ))
