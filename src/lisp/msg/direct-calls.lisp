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

(defun %populate-command-options (pr &key id id-sub &allow-other-keys)
  (when (zerop id-sub) (error "Zero id-sub passed to root-enode-populate-command-options!"))
  (mopr-sgt:with-bound-procedure-accessors ((root mopr-sgt:procedure-root)) pr
    (let* ((n (mopr-msg/ctrl:find-enode-by-id root id))
           (opts (mopr-msg/ctrl:payload-get-options (mopr-sgt:bnode-find-payload n) (1- id-sub)))
           (nof-opts (length opts)))
      (xmls:make-node
       :name "options" :attrs `(("id" ,id) ("id-sub" ,id-sub))
       :children (loop for o in opts
                       collecting (xmls:make-node :name "option" :attrs `(("name" ,o))))))))

(defun populate-command-options (query)
  (apply #'%populate-command-options *procedure* query))

(defun apply-command-option (id id-sub id-opt)
  (mopr-viz/repr:%apply-command-option *procedure* id id-sub id-opt))
