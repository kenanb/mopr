;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

;; HUNCHENTOOT BASED LOCALHOST SESSION
;;
;; For Loopback-HTTP backend, communication between frontend and backend happens
;; via HTTP over TCP/IP using loopback (127.0.0.1) address. Port number to use
;; needs to be provided for backend initialization.

(in-package :cl-user)

(defpackage :mopr-srv/loopback-http
  (:import-from :mopr-msg)
  (:import-from :hunchentoot)
  (:use #:cl)
  (:export
   #:backend-init
   #:backend-term
   ))

(in-package #:mopr-srv/loopback-http)

(defconstant +content-type+ "application/xml; charset=utf-8")

(defmacro ht-debug-print (&body body)
  (append
   '(prog1 nil)
   (loop for expr in body
         collecting `(format t "~%~A: ~S" ,(format nil "~S" expr) ,expr))
   '((format t "~%"))))

(defun dispatch-root ()
  ;; (ht-debug-print
  ;;   (hunchentoot:cookies-in*)
  ;;   (hunchentoot:request-method*)
  ;;   (hunchentoot:request-uri*)
  ;;   (hunchentoot:headers-in*))
  (setf (hunchentoot:content-type*) +content-type+)
  (case (hunchentoot:request-method*)
    (:get
     (mopr-msg:request-handler-get
      (hunchentoot:request-uri*)))
    (:post
     (unless (string= +content-type+ (hunchentoot:header-in* :content-type))
       (error "Unsupported request content type!"))
     (mopr-msg:request-handler-post
      (hunchentoot:request-uri*)
      (hunchentoot:raw-post-data :force-text t
                                 :external-format :utf-8)))))

(setf hunchentoot:*dispatch-table*
      (list (hunchentoot:create-prefix-dispatcher "/" #'dispatch-root)))

(defvar *server* nil)

(defun backend-init (wdir-abs port)
  (format t "MOPR initializing loopback-http backend.~%")
  (format t "Acquiring workshop lock at path: ~A~%" wdir-abs)

  (mopr-msg:acquire-ws wdir-abs)
  (setf *server* (make-instance 'hunchentoot:easy-acceptor
                                :address "127.0.0.1"
                                :port port))
  (hunchentoot:start *server*)
  (setf mopr-msg:*messaging-session* (make-instance 'mopr-msg:messaging-session)))

(defun backend-term ()
  (format t "MOPR terminating loopback-http backend.~%")
  (format t "Releasing workshop lock.~%")

  (setf mopr-msg:*messaging-session* nil)
  (hunchentoot:stop *server*)
  (setf *server* nil)
  (mopr-msg:release-ws))
