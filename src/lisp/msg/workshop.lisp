;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defvar *workshop* nil)

(defvar *workshop-lock* (bt:make-lock))

(defun acquire-ws (wdir-abs &aux (wcons (mopr-org:load-workshop-manifest wdir-abs)))
  (prog1 nil
    (mopr-org:workshop-set-lock-state-or-fail wcons :acquired)
    (setf *workshop* wcons)))

(defun release-ws (&aux (wcons *workshop*))
  (prog1 nil
    (setf *workshop* nil)
    (mopr-org:workshop-set-lock-state-or-fail wcons :released)))

(defun ws-bound-p ()
  (if *workshop* t nil))

(defun ws-descriptor ()
  (car *workshop*))

(defun ws-projects ()
  (bt:with-lock-held (*workshop-lock*)
    (mopr-org:workshop-info-projects (cdr *workshop*))))

(defun ws-sessions ()
  (bt:with-lock-held (*workshop-lock*)
    (mopr-org:workshop-info-sessions (cdr *workshop*))))

(defun ws-create-project (pdir-rel &rest ctor-kwargs &key &allow-other-keys)
  (bt:with-lock-held (*workshop-lock*)
    (apply #'mopr-org:workshop-create-project *workshop* pdir-rel ctor-kwargs)))

(defun ws-acquire-project (lookup-type lookup-val session-id)
  (bt:with-lock-held (*workshop-lock*)
    (mopr-org:workshop-acquire-project *workshop* lookup-type lookup-val session-id)))

(defun ws-release-project (lookup-type lookup-val session-id)
  (bt:with-lock-held (*workshop-lock*)
    (mopr-org:workshop-release-project *workshop* lookup-type lookup-val session-id)))
