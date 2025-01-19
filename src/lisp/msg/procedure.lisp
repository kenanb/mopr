;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

;;
;;; SINGLETON
;;

(defconstant +component-classes+ '(mopr-ops:node-identifier
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

(defun populate-editor-layout (&key (pixels-w 480) (pixels-h 960) &allow-other-keys)
  (mopr-viz/repr:enode-procedure-calculate-editor-layout *procedure*
                                                         (float pixels-w)
                                                         (float pixels-h)))

(defun populate-command-options (&key (id-node 0) (id-sub 0) &allow-other-keys)
  (mopr-ops:enode-procedure-calculate-command-options *procedure* id-node id-sub))

(defun apply-command-option (id-node id-sub id-opt)
  (mopr-ops:enode-procedure-apply-command-option *procedure* id-node id-sub id-opt))
