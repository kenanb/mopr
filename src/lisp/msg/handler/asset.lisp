;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

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
           (apath (mopr-org:desc-chain-as-path (mopr-uri:make-desc-chain pdesc adesc)
                                               :relative-expected-p t
                                               :file-expected-p t)))
      (xmls:make-node :name "asset" :attrs `(("path" ,(namestring apath)))))))

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
           (apath (mopr-org:desc-chain-as-path (mopr-uri:make-desc-chain pdesc adesc)
                                               :relative-expected-p t
                                               :file-expected-p t))
           (apath-full (mopr-org:desc-chain-as-path (mopr-uri:make-desc-chain (ws-descriptor) pdesc adesc)
                                                    :file-expected-p t))
           (ret (handle-get-request rid category :context context :remaining remaining)))
      (cond
        ((equal (xmls:node-name request-body) "action")
         (let ((action-name (cadr (assoc "name" (xmls:node-attrs request-body) :test #'string=))))
           (cond
             ((equal action-name "bind")
              (bind-pr apath-full)
              (format t "File bound: ~A~%" apath)
              (push `("uri" ,(format nil "/workshop/~A/project/~A/asset/~A/working/0/" wuuid puuid auuid))
                    (xmls:node-attrs ret))
              ret))))
        (t ret)))))
