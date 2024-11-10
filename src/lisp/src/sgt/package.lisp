;;;; package.lisp

(defpackage :mopr-sgt
  (:use #:cl)
  (:export

   ;; ENODE API
   #:enode
   #:enode-parent
   #:enode-children
   #:enode-extensions

   ;; Generic APIs
   #:enode-find-extension
   #:enode-initialize-extension
   #:enode-initialize-extensions-recursive
   #:debug-print

   ;; Main ENODE Categories
   #:execution-enode
   #:container-enode
   #:directive-enode

   ;; ROOT-ENODE
   #:root-enode

   ;; GROUP-ENODE
   #:group-enode

   ;; VAR-ENODE
   #:var-enode
   #:var-enode-name-param
   #:var-enode-aux-form-param
   #:var-enode-val-form-param

   ;; EACH-ENODE
   #:each-enode
   #:each-enode-name-param
   #:each-enode-keys-form-param
   #:each-enode-vals-form-param

   ;; IOTA-ENODE
   #:iota-enode
   #:iota-enode-name-param
   #:iota-enode-key-param
   #:iota-enode-end-param
   #:iota-enode-start-param
   #:iota-enode-step-param

   ;; CALL-ENODE
   #:call-enode
   #:call-enode-aux-form-param
   #:call-enode-body-form-param

   ;; PRIM-CALL-ENODE
   #:prim-call-enode

   ;; PRIM-SCHEMA-PROP-ENODE
   #:prim-schema-prop-enode
   #:prim-schema-prop-enode-info-param
   #:prim-schema-prop-enode-body-form-param

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

   ;; CALLABLES
   #:make-prop
   #:make-group

   ;; ;; ENODE-COPY
   ;; #:copy-enode-instance

   ;; ;; ENODE-PREPROCESS
   ;; #:preprocess-all
   ;; #:preprocess-all-call-enabled

   ;; ENODE-EXECUTE
   #:populate-layer

   ;; ENODE-SERIALIZE
   #:deserialize
   #:enode-serialize

   ))
