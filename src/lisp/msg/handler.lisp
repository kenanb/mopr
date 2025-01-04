;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defun failure-response (verb rid category context remaining reason)
  (declare (ignore rid))
  (format t "~A request ~A:
CATEGORY  : ~A
CONTEXT   : ~S
REMAINING : ~S~%"
          verb reason category context remaining)
  (xmls:make-node :name "unhandled-request"
                  :attrs `(("verb" ,(symbol-name verb))
                           ("reason" ,reason)
                           ("category" ,(symbol-name category)))))

(defgeneric handle-get-request (rid category &key context remaining)

  (:method (rid category &key context remaining)
    (failure-response :get rid category context remaining "unimplemented"))

  (:method (rid (category (eql 'base-request-fn-top))
            &key context remaining)
    (declare (ignore rid category context remaining))
    (error "The GET request handle BASE-REQUEST-FN-TOP should be unreachable with the current design!"))

  (:method (rid (category (eql 'request-fn-relative))
            &key context remaining)
    (declare (ignore rid category context remaining))
    (error "The GET request handle REQUEST-FN-RELATIVE is currently unsupported!"))

  (:method (rid (category (eql 'request-fn-unknown))
            &key context remaining)
    (failure-response :get rid category context remaining "unsupported")))

(defmethod handle-get-request (rid (category (eql 'main-request-fn-root-ep))
                               &key context remaining)
  (declare (ignore rid context remaining))
  (xmls:make-node
   :name "endpoints"
   :children
   (list (xmls:make-node :name "endpoint" :attrs `(("name" "workshop")
                                                   ("uri" "/workshop/"))))))

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

(defmethod handle-get-request (rid (category (eql 'main-request-fn-asset-ep))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (getf context :project-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (let* ((pcons (mopr-uri:desc-alist-assoc (ws-projects) :uuid puuid))
           (pinfo (cdr pcons))
           (project-assets (mopr-org:project-info-assets pinfo)))
      (xmls:make-node
       :name "assets"
       :children
       (loop for a in project-assets
             for auuid = (mopr-uri:descriptor-uuid (car a))
             for path = (namestring (mopr-org:pndescriptor-path (car a)))
             for uri = (format nil "/workshop/~A/project/~A/asset/~A/" wuuid puuid auuid)
             collecting (xmls:make-node :name "asset"
                                        :attrs `(("path" ,path)
                                                 ("uuid" ,auuid)
                                                 ("uri" ,uri))))))))

(defmethod handle-get-request (rid (category (eql 'main-request-fn-asset-res))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (getf context :project-res))
        (auuid (getf context :asset-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (let* ((ep-uri-staging (format nil "/workshop/~A/project/~A/asset/~A/staging/" wuuid puuid auuid))
           (ep-uri-working (format nil "/workshop/~A/project/~A/asset/~A/working/" wuuid puuid auuid)))
      (xmls:make-node
       :name "endpoints"
       :children
       (list (xmls:make-node :name "endpoint"
                             :attrs `(("name" "staging")
                                      ("uri" ,ep-uri-staging)))
             (xmls:make-node :name "endpoint"
                             :attrs `(("name" "working")
                                      ("uri" ,ep-uri-working))))))))

(defmethod handle-get-request (rid (category (eql 'base-request-fn-staging))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (messaging-session-puuid *messaging-session*))
        (auuid (getf context :asset-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (unless (equal puuid (getf context :project-res))
      (error "Project ID doesn't match the active project."))
    (let* ((pcons (mopr-uri:desc-alist-assoc (ws-projects) :uuid puuid))
           (pdesc (car pcons))
           (acons (mopr-uri:desc-alist-assoc (mopr-org:project-info-assets (cdr pcons)) :uuid auuid))
           (adesc (car acons))
           (apath (mopr-org:desc-chain-as-path (mopr-uri:make-desc-chain (ws-descriptor) pdesc adesc)
                                               :file-expected-p t)))
      (xmls:make-node :name "asset" :attrs `(("path" ,(namestring apath)))))))

(defgeneric handle-post-request (rid request-body category &key context remaining)

  (:method (rid request-body category &key context remaining)
    (format t "POST REQUEST BODY:~%~A~%" request-body)
    (failure-response :post rid category context remaining "unimplemented"))

  (:method (rid request-body (category (eql 'base-request-fn-top))
            &key context remaining)
    (declare (ignore rid request-body category context remaining))
    (error "The POST request handle BASE-REQUEST-FN-TOP should be unreachable with the current design!"))

  (:method (rid request-body (category (eql 'request-fn-relative))
            &key context remaining)
    (declare (ignore rid request-body category context remaining))
    (error "The POST request handle REQUEST-FN-RELATIVE is currently unsupported!"))

  (:method (rid request-body (category (eql 'request-fn-unknown))
            &key context remaining)
    (format t "POST REQUEST BODY:~%~A~%" request-body)
    (failure-response :post rid category context remaining "unsupported")))

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

(defmethod handle-post-request (rid request-body (category (eql 'base-request-fn-staging))
                                &key context remaining)
  (declare (ignore rid category))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (messaging-session-puuid *messaging-session*))
        (auuid (getf context :asset-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (unless (equal puuid (getf context :project-res))
      (error "Project ID doesn't match the active project."))
    (let* ((pcons (mopr-uri:desc-alist-assoc (ws-projects) :uuid puuid))
           (pdesc (car pcons))
           (acons (mopr-uri:desc-alist-assoc (mopr-org:project-info-assets (cdr pcons)) :uuid auuid))
           (adesc (car acons))
           (apath (mopr-org:desc-chain-as-path (mopr-uri:make-desc-chain (ws-descriptor) pdesc adesc)
                                               :file-expected-p t))
           (ret (handle-get-request rid category :context context :remaining remaining)))
      (cond
        ((equal (xmls:node-name request-body) "action")
         (let ((action-name (cadr (assoc "name" (xmls:node-attrs request-body) :test #'string=))))
           (cond
             ((equal action-name "bind")
              (bind-repr apath)
              (format t "File bound: ~A~%" apath)
              (push `("uri" ,(format nil "/workshop/~A/project/~A/asset/~A/working/0/" wuuid puuid auuid))
                    (xmls:node-attrs ret))
              ret))))
        (t ret)))))

(defmethod handle-post-request (rid request-body (category (eql 'main-request-fn-working-res))
                                &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (messaging-session-puuid *messaging-session*))
        (auuid (getf context :asset-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (unless (equal puuid (getf context :project-res))
      (error "Project ID doesn't match the active project."))
    (cond
      ((equal (xmls:node-name request-body) "action")
       (let ((action-name (cadr (assoc "name" (xmls:node-attrs request-body) :test #'string=))))
         (cond
           ((equal action-name "init-repr")
            (init-repr)
            (format t "Representation components for bound procedure are initialized.~%"))
           ((equal action-name "term-repr")
            (term-repr)
            (format t "Representation components for bound procedure are terminated.~%"))))))
    ;; TODO : Revisit.
    (xmls:make-node :name "action-result" :attrs `(("status" "OK")))))

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
