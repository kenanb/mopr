;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defmethod handle-get-request (rid (category (eql 'main-request-fn-project-ep))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor))))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (xmls:make-node
     :name "projects"
     :children
     (loop for p in (ws-projects)
           for puuid = (mopr-uri:descriptor-uuid (car p))
           for path = (namestring (mopr-org:pndescriptor-path (car p)))
           for uri = (format nil "/workshop/~A/project/~A/" wuuid puuid)
           collecting (xmls:make-node :name "project"
                                      :attrs `(("path" ,path)
                                               ("uuid" ,puuid)
                                               ("uri" ,uri)))))))

(defmethod handle-get-request (rid (category (eql 'main-request-fn-project-res))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (getf context :project-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (let ((ep-uri-asset (format nil "/workshop/~A/project/~A/asset/" wuuid puuid))
          (ep-uri-lock (format nil "/workshop/~A/project/~A/lock/" wuuid puuid)))
      (xmls:make-node
       :name "endpoints"
       :children
       (list (xmls:make-node :name "endpoint"
                             :attrs `(("name" "asset")
                                      ("uri" ,ep-uri-asset)))
             (xmls:make-node :name "endpoint"
                             :attrs `(("name" "lock")
                                      ("uri" ,ep-uri-lock))))))))

(defmethod handle-get-request (rid (category (eql 'main-request-fn-project-lock-ep))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (getf context :project-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (xmls:make-node :name "project-lock"
                    :attrs `(("state" ,(if (gethash puuid (ws-sessions))
                                           "acquired" "released"))))))

(defmethod handle-post-request (rid request-body (category (eql 'main-request-fn-project-lock-ep))
                                &key context remaining)
  (declare (ignore rid))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (getf context :project-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (cond
      ((equal (xmls:node-name request-body) "action")
       (let ((action-name (cadr (assoc "name" (xmls:node-attrs request-body) :test #'string=)))
             (sid (messaging-session-id *messaging-session*)))
         (cond
           ((equal action-name "acquire")
            (ws-acquire-project :uuid puuid sid)
            (setf (messaging-session-puuid *messaging-session*) puuid)
            (format t "Session acquired project lock.~%"))
           ((equal action-name "release")
            (setf (messaging-session-puuid *messaging-session*) nil)
            (ws-release-project :uuid puuid sid)
            (format t "Session released project lock.~%"))))))
    (handle-get-request rid category :context context :remaining remaining)))
