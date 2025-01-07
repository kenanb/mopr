;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

;; NOTE : Current XMLS seems to fail escaping Newline characters in
;; XML attribute values during XMLS:TOXML, which means decoder will
;; (correctly) normalize those newlines to spaces. I locally patched
;; XMLS::ESCAPE-FOR-HTML so that it maps Newline character to "&#010;"
;; encoding.

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
