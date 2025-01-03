;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-srv)

;; IN-PROCESS SESSION
;;
;; For in-process backend, there is a single client that lives within the same
;; process. So we just bind the session during backend initialization, and
;; unbind during backend termination.

(defun in-process-backend-init (wdir-abs)
  (format t "MOPR initializing in-process backend.~%")
  (format t "Acquiring workshop lock at path: ~A~%" wdir-abs)
  (mopr-msg:acquire-ws wdir-abs)
  (setf mopr-msg:*messaging-session* (make-instance 'mopr-msg:messaging-session)))

(defun in-process-backend-term ()
  (format t "MOPR terminating in-process backend.~%")
  (format t "Releasing workshop lock.~%")
  (setf mopr-msg:*messaging-session* nil)
  (mopr-msg:release-ws))

(defun in-process-backend-handle-get-request (response-body-h uri-str)
  (let* ((response-str (mopr-msg:request-handler-get uri-str))
         (response-f (cffi:foreign-string-alloc response-str)))
    ;; (format t "L                  | RESPONSE ADDRESS : ~A~%" response-body-h)
    ;; (format t "L                  | ALLOCATED STRING : ~A~%" response-f)
    ;; (format t "L <SETF            | RESPONSE MEM-REF : ~A~%"
    ;;         (cffi:mem-ref response-body-h :pointer))
    (setf (cffi:mem-ref response-body-h :pointer) response-f)
    ;; (format t "L            SETF> | RESPONSE MEM-REF : ~A~%"
    ;;         (cffi:mem-ref response-body-h :pointer))
    ))

(defun in-process-backend-handle-post-request (response-body-h uri-str request-body-str)
  (let* ((response-str (mopr-msg:request-handler-post uri-str request-body-str))
         (response-f (cffi:foreign-string-alloc response-str)))
    ;; (format t "L                  | RESPONSE ADDRESS : ~A~%" response-body-h)
    ;; (format t "L                  | ALLOCATED STRING : ~A~%" response-f)
    ;; (format t "L <SETF            | RESPONSE MEM-REF : ~A~%"
    ;;         (cffi:mem-ref response-body-h :pointer))
    (setf (cffi:mem-ref response-body-h :pointer) response-f)
    ;; (format t "L            SETF> | RESPONSE MEM-REF : ~A~%"
    ;;         (cffi:mem-ref response-body-h :pointer))
    ))

(defun in-process-backend-release-response (response-body-h)
  (let* ((response-f (cffi:mem-ref response-body-h :pointer)))
    ;; (format t "L                  | RESPONSE ADDRESS : ~A~%" response-body-h)
    (cffi:foreign-string-free response-f)
    ;; (format t "L <SETF            | RESPONSE MEM-REF : ~A~%"
    ;;         (cffi:mem-ref response-body-h :pointer))
    (setf (cffi:mem-ref response-body-h :pointer) (cffi:null-pointer))
    ;; (format t "L            SETF> | RESPONSE MEM-REF : ~A~%"
    ;;         (cffi:mem-ref response-body-h :pointer))
    ))