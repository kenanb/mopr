;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defvar *workshop* nil)

(defvar *workshop-lock* (bt:make-lock))

(defun acquire-ws (directory &aux (ws (mopr-res:load-workshop-manifest directory)))
  (prog1 nil
    (mopr-res:workshop-set-lock-state-or-fail ws :acquired)
    (setf *workshop* ws)))

(defun release-ws (&aux (ws *workshop*))
  (prog1 nil
    (setf *workshop* nil)
    (mopr-res:workshop-set-lock-state-or-fail ws :released)))

(defun ws-bound-p ()
  (if *workshop* t nil))

(defun ws-uuid ()
  (mopr-res:workshop-uuid *workshop*))

(defun ws-location ()
  (mopr-res:workshop-location *workshop*))

(defun ws-projects ()
  (bt:with-lock-held (*workshop-lock*)
    (mopr-res:workshop-projects *workshop*)))

(defun ws-project-assignments ()
  (bt:with-lock-held (*workshop-lock*)
    (mopr-res:workshop-project-assignments *workshop*)))

(defun ws-create-project (rel-project-directory)
  (bt:with-lock-held (*workshop-lock*)
    (mopr-res:workshop-create-project *workshop* rel-project-directory)))

(defun ws-acquire-project (lookup-type lookup-val session-id)
  (bt:with-lock-held (*workshop-lock*)
    (mopr-res:workshop-acquire-project *workshop* lookup-type lookup-val session-id)))

(defun ws-release-project (lookup-type lookup-val session-id)
  (bt:with-lock-held (*workshop-lock*)
    (mopr-res:workshop-release-project *workshop* lookup-type lookup-val session-id)))
