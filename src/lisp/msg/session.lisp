;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defconstant +component-classes+ '(mopr-ops:node-identifier
                                   mopr-viz/repr-rnode:rnode))

(defvar *messaging-session* nil
  "This special variable will be dynamically bound to a per-client
MESSAGING-SESSION instance during request handler execution.")

(defclass messaging-session ()
  ((id
    :type integer
    :initform 0
    :accessor messaging-session-id
    :documentation "Session ID.")
   (puuid
    :type (or null string)
    :initform nil
    :accessor messaging-session-puuid
    :documentation "UUID of the project currently locked by this session.")
   (assets
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :reader messaging-session-assets
    :documentation "Map of asset UUID to runtime representation of assets.")))

(defun bind-pr (auuid filepath)
  (let ((pr (mopr-sgt:make-cnode-procedure-from-usds-file filepath)))
    (setf (gethash auuid (messaging-session-assets *messaging-session*))
          (mopr-sgt:make-enode-procedure pr))))

(defun pr-execute (auuid layer-h call-enabled)
  (mopr-exe:procedure-execute
   (gethash auuid (messaging-session-assets *messaging-session*))
   layer-h call-enabled))

(defun pr-export-to-usd-file (auuid filepath call-enabled)
  (mopr-exe:procedure-export-to-usd-file
   (gethash auuid (messaging-session-assets *messaging-session*))
   filepath call-enabled))

(defun pr-init-interaction (auuid)
  (mopr-sgt:enode-procedure-create-components
   (gethash auuid (messaging-session-assets *messaging-session*))
   +component-classes+))

(defun pr-term-interaction (auuid)
  (mopr-sgt:enode-procedure-delete-components
   (gethash auuid (messaging-session-assets *messaging-session*))
   +component-classes+))

(defun pr-populate-editor-layout (auuid &key (pixels-w 480) (pixels-h 960)
                                  &allow-other-keys)
  (mopr-viz/repr:enode-procedure-calculate-editor-layout
   (gethash auuid (messaging-session-assets *messaging-session*))
   (float pixels-w)
   (float pixels-h)))

(defun pr-populate-command-options (auuid &key (id-node 0) (id-sub 0)
                                    &allow-other-keys)
  (mopr-ops:enode-procedure-calculate-command-options
   (gethash auuid (messaging-session-assets *messaging-session*))
   id-node id-sub))

(defun pr-apply-command-option (auuid id-node id-sub id-opt)
  (mopr-ops:enode-procedure-apply-command-option
   (gethash auuid (messaging-session-assets *messaging-session*))
   id-node id-sub id-opt))
