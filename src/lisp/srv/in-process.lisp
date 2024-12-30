;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-srv)

(defun in-process-backend-init (wdir-abs)
  (format t "MOPR initializing in-process backend.~%")
  (format t "Acquiring workshop lock at path: ~A~%" wdir-abs)
  (mopr-msg:acquire-ws wdir-abs))

(defun in-process-backend-term ()
  (format t "MOPR terminating in-process backend.~%")
  (format t "Releasing workshop lock.~%")
  (mopr-msg:release-ws))

(defun in-process-backend-handle-get-request (response-h uri-str)
  (let* ((response (mopr-msg:request-handler-get uri-str))
         (response-f (cffi:foreign-string-alloc response)))
    ;; (format t "L                  | RESPONSE ADDRESS : ~A~%" response-h)
    ;; (format t "L                  | ALLOCATED STRING : ~A~%" response-f)
    ;; (format t "L <SETF            | RESPONSE MEM-REF : ~A~%" (cffi:mem-ref response-h :pointer))
    (setf (cffi:mem-ref response-h :pointer) response-f)
    ;; (format t "L            SETF> | RESPONSE MEM-REF : ~A~%" (cffi:mem-ref response-h :pointer))
    ))

(defun in-process-backend-release-response (response-h)
  (let* ((response-f (cffi:mem-ref response-h :pointer)))
    ;; (format t "L                  | RESPONSE ADDRESS : ~A~%" response-h)
    (cffi:foreign-string-free response-f)
    ;; (format t "L <SETF            | RESPONSE MEM-REF : ~A~%" (cffi:mem-ref response-h :pointer))
    (setf (cffi:mem-ref response-h :pointer) (cffi:null-pointer))
    ;; (format t "L            SETF> | RESPONSE MEM-REF : ~A~%" (cffi:mem-ref response-h :pointer))
    ))
