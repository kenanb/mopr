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

(defun destruct-command-options (cmd-options-ptr)
  (mopr-viz/repr:%destruct-command-options cmd-options-ptr))

(defun populate-command-options (cmd-options-ptr id id-sub)
  (mopr-viz/repr:%populate-command-options *procedure* cmd-options-ptr id id-sub))

(defun apply-command-option (id id-sub id-opt)
  (mopr-viz/repr:%apply-command-option *procedure* id id-sub id-opt))
