;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defmethod handle-get-request (rid (category (eql 'main-request-fn-workshop-ep))
                               &key context remaining)
  (declare (ignore rid category context remaining))
  (let* ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
         (uri (format nil "/workshop/~A/" wuuid)))
    (xmls:make-node
     :name "workshop"
     :attrs `(("uuid" ,wuuid)
              ("uri" ,uri)))))

(defmethod handle-get-request (rid (category (eql 'main-request-fn-workshop-res))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor))))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (let* ((uri (format nil "/workshop/~A/project/" wuuid)))
      (xmls:make-node
       :name "endpoints"
       :children
       (list (xmls:make-node :name "endpoint"
                             :attrs `(("name" "project")
                                      ("uri" ,uri))))))))
