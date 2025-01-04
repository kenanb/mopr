;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defun request-handler-get (uri-str)
  (let* ((rid (make-instance 'mopr-uri:resource-id :str uri-str)))
    ;; (format t "uri: ~S~%     ~S~%     ~S~%"
    ;;         (mopr-uri:resource-id-data rid)
    ;;         (mopr-uri:resource-id-path rid)
    ;;         (mopr-uri:resource-id-query rid))
    (xmls:toxml
     (apply #'handle-get-request rid
            (dispatch-path (mopr-uri:resource-id-path rid) +dispatch-tree+)))))

(defun request-handler-post (uri-str request-body-str)
  (let* ((rid (make-instance 'mopr-uri:resource-id :str uri-str)))
    ;; (format t "uri: ~S~%     ~S~%     ~S~%"
    ;;         (mopr-uri:resource-id-data rid)
    ;;         (mopr-uri:resource-id-path rid)
    ;;         (mopr-uri:resource-id-query rid))
    (xmls:toxml
     (apply #'handle-post-request rid (xmls:parse request-body-str)
            (dispatch-path (mopr-uri:resource-id-path rid) +dispatch-tree+)))))
