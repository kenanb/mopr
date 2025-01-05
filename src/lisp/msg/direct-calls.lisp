;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

;;
;;; SINGLETON
;;

(defconstant +component-classes+ '(mopr-msg/ctrl:node-identifier
                                   mopr-viz/repr-rnode:rnode))

(defvar *procedure* nil)

(defun bind-repr (filepath)
  (let ((pr (mopr-sgt:make-cnode-procedure-from-usds-file filepath)))
    (setf *procedure* (mopr-sgt:make-enode-procedure pr))))

(defun exec-repr (layer-h call-enabled)
  (mopr-exe:procedure-execute *procedure* layer-h call-enabled))

(defun init-repr ()
  (mopr-sgt:enode-procedure-create-components *procedure* +component-classes+))

(defun term-repr ()
  (mopr-sgt:enode-procedure-delete-components *procedure* +component-classes+))

(defun populate-command-queue (cmd-queue-ptr)
  (mopr-viz/repr:%populate-command-queue *procedure* cmd-queue-ptr))

(defun destruct-command-queue (cmd-queue-ptr)
  (mopr-viz/repr:%destruct-command-queue cmd-queue-ptr))

(defun %populate-editor-layout (pr &key pixels-w pixels-h &allow-other-keys)
  (mopr-sgt:with-bound-procedure-accessors ((root mopr-sgt:procedure-root)) pr
    (xmls:make-node
     :name "layout" :attrs `(("pixels-w" ,pixels-w) ("pixels-h" ,pixels-h)))))

(defun populate-editor-layout (query)
  (apply #'%populate-editor-layout *procedure* query))

(defun %populate-command-options (pr &key id-node id-sub &allow-other-keys)
  (when (zerop id-sub) (error "Zero id-sub passed to root-enode-populate-command-options!"))
  (mopr-sgt:with-bound-procedure-accessors ((root mopr-sgt:procedure-root)) pr
    (let* ((n (mopr-msg/ctrl:find-enode-by-id root id-node))
           (opts (mopr-msg/ctrl:payload-get-options (mopr-sgt:bnode-find-payload n) (1- id-sub)))
           (nof-opts (length opts)))
      (xmls:make-node
       :name "options" :attrs `(("id-node" ,id-node) ("id-sub" ,id-sub))
       :children (loop for o in opts
                       collecting (xmls:make-node :name "option" :attrs `(("name" ,o))))))))

(defun populate-command-options (query)
  (apply #'%populate-command-options *procedure* query))

(defun %apply-command-option (pr id-node id-sub id-opt)
  (when (zerop id-sub) (error "Zero id-sub passed to root-enode-apply-command-option!"))
  (when (zerop id-opt) (error "Zero id-opt passed to root-enode-apply-command-option!"))
  (mopr-sgt:with-bound-procedure-accessors ((root mopr-sgt:procedure-root)) pr
    (let* ((n (mopr-msg/ctrl:find-enode-by-id root id-node))
           (opts (mopr-msg/ctrl:payload-get-options (mopr-sgt:bnode-find-payload n) (1- id-sub)))
           (idx (1- id-opt)))
      (format t "APPLIED OPTION: ~A~%" (nth idx opts)))))

(defun apply-command-option (&rest args)
  (apply #'%apply-command-option *procedure* args))
