;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-uri)

(defclass resource-id ()
  ((str
    :initarg :str
    :initform (error "RESOURCE-ID cannot be initialized without a URI string.")
    :reader resource-id-str)
   (data :reader resource-id-data)
   (path :reader resource-id-path)
   (query :reader resource-id-query)))

(defmethod initialize-instance :after ((instance resource-id) &rest initargs)
  "All values are generated here out of the URI assignment."
  (declare (ignore initargs))
  (with-slots (str data path query)
      instance
    (setf data (quri:uri str))
    (setf path (split-sequence:split-sequence #\/ (quri:uri-path data)))
    (setf query (loop for (k . v) in (quri:uri-query-params data)
                      for kw = (alexandria:make-keyword (string-upcase k))
                      appending (list kw (case kw
                                           ((:id-node :id-sub :pixels-w :pixels-h)
                                            (parse-integer v))
                                           (t v)))))))
