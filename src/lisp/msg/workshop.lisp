;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defvar *workshop* nil)

(defvar *workshop-lock* (bt:make-lock))

(defun acquire-ws (wdir-abs &aux (wdesc (mopr-org:load-workshop-metadata wdir-abs)))
  (setf *workshop* (mopr-org:acquire-workshop wdesc)))

(defun release-ws (&aux (wdesc (car *workshop*)))
  (setf *workshop* (mopr-org:release-workshop wdesc)))

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
    (apply #'mopr-org:workshop-create-project
           (mopr-uri:make-desc-chain (car *workshop*))
           (cdr *workshop*)
           pdir-rel ctor-kwargs)))

(defun ws-acquire-project (lookup-type lookup-val session-id)
  (bt:with-lock-held (*workshop-lock*)
    (mopr-org:workshop-acquire-project (cdr *workshop*) lookup-type lookup-val session-id)))

(defun ws-release-project (lookup-type lookup-val session-id)
  (bt:with-lock-held (*workshop-lock*)
    (mopr-org:workshop-release-project (cdr *workshop*) lookup-type lookup-val session-id)))
