;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defun request-handler-get (uri-str)
  (let* ((uri (make-instance 'mopr-uri:resource-id :str uri-str)))
    (format t "uri: ~S~%     ~S~%     ~S~%"
            (mopr-uri:resource-id-data uri)
            (mopr-uri:resource-id-path uri)
            (mopr-uri:resource-id-query uri))
    (xmls:toxml (xmls:make-node :name "mopr-messaging-test"))))
