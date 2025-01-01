;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defconstant +dispatch-tree+
  '(:top base-get-fn-top
    ("" :root-ep base-get-fn-root-ep
     ("" . main-get-fn-root-ep)
     ("workshop" :workshop-res base-get-fn-workshop-ep
      ("" . main-get-fn-workshop-ep)
      (t :workshop-res-ep base-get-fn-workshop-res
       ("" . main-get-fn-workshop-res)
       ("project" :project-res base-get-fn-project-ep
        ("" . main-get-fn-project-ep)
        (t :project-res-ep base-get-fn-project-res
         ("" . main-get-fn-project-res)
         ("asset" :asset-res base-get-fn-asset-ep
                  ("" . main-get-fn-asset-ep)
                  (t :asset-res-ep base-get-fn-asset-res
                     ("" . main-get-fn-asset-res)
                     ("worktree" . base-get-fn-worktree)))))))
     (t . get-fn-unknown))
    (t . get-fn-relative)))

(defun failure-response (rid category context remaining reason)
  (declare (ignore rid))
  (format t "~%GET request ~A:
CATEGORY  : ~A
CONTEXT   : ~S
REMAINING : ~S~%"
          reason category context remaining)
  (xmls:make-node :name "unhandled-request"))

(defgeneric handle-get-request (rid category &key context remaining)

  (:method (rid category &key context remaining)
    (failure-response rid category context remaining "unimplemented"))

  (:method (rid (category (eql 'base-get-fn-top))
            &key context remaining)
    (declare (ignore rid category context remaining))
    (error "The GET request handle BASE-GET-FN-TOP should be unreachable with the current design!"))

  (:method (rid (category (eql 'get-fn-relative))
            &key context remaining)
    (declare (ignore rid category context remaining))
    (error "The GET request handle GET-FN-RELATIVE is currently unsupported!"))

  (:method (rid (category (eql 'get-fn-unknown))
            &key context remaining)
    (failure-response rid category context remaining "unsupported")))

(defmethod handle-get-request (rid (category (eql 'main-get-fn-root-ep))
                               &key context remaining)
  (declare (ignore rid context remaining))
  (xmls:make-node
   :name "endpoints"
   :children
   (list (xmls:make-node :name "endpoint" :attrs `(("name" "workshop")
                                                   ("uri" "/workshop/"))))))

(defmethod handle-get-request (rid (category (eql 'main-get-fn-workshop-ep))
                               &key context remaining)
  (declare (ignore rid category context remaining))
  (let* ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
         (uri (format nil "/workshop/~A/" wuuid)))
    (xmls:make-node
     :name "workshop"
     :attrs `(("uuid" ,wuuid)
              ("uri" ,uri)))))

(defmethod handle-get-request (rid (category (eql 'main-get-fn-workshop-res))
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

(defmethod handle-get-request (rid (category (eql 'main-get-fn-project-ep))
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

(defmethod handle-get-request (rid (category (eql 'main-get-fn-project-res))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (getf context :project-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (let* ((uri (format nil "/workshop/~A/project/~A/asset/" wuuid puuid)))
      (xmls:make-node
       :name "endpoints"
       :children
       (list (xmls:make-node :name "endpoint"
                             :attrs `(("name" "asset")
                                      ("uri" ,uri))))))))

(defmethod handle-get-request (rid (category (eql 'main-get-fn-asset-ep))
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

(defmethod handle-get-request (rid (category (eql 'main-get-fn-asset-res))
                               &key context remaining)
  (declare (ignore rid category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (getf context :project-res))
        (auuid (getf context :asset-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (let* ((uri (format nil "/workshop/~A/project/~A/asset/~A/worktree/" wuuid puuid auuid)))
      (xmls:make-node
       :name "endpoints"
       :children
       (list (xmls:make-node :name "endpoint"
                             :attrs `(("name" "worktree")
                                      ("uri" ,uri))))))))

(defun dispatch-path (keyform-list handler)
  "DISPATCH-PATH keyform-list handler => (fn . context)
handler::= { ancestral-handler | innermost-handler }
ancestral-handler::= (context-parameter fn clause*)
innermost-handler::= { (context-parameter fn) | fn }
clause::= (key . handler)
key ::= { t | match-string }

Atomic form of innermost-handler is syntactic sugar for the list form (:THIS fn).
This allows visually treating dot as a placeholder for :THIS in innermost-handler.
"
  (loop for ht = handler
          then (loop for (m . hn) in dp if (or (eq m t) (equal m v)) return hn)
        for (k dpm . dp) = (if (consp ht) ht (list :this ht))
        for (v . r) on keyform-list appending (list k v) into context while dp
        finally (return (list dpm :context context :remaining r))))

(defun request-handler-get (uri-str)
  (let* ((rid (make-instance 'mopr-uri:resource-id :str uri-str)))
    ;; (format t "uri: ~S~%     ~S~%     ~S~%"
    ;;         (mopr-uri:resource-id-data rid)
    ;;         (mopr-uri:resource-id-path rid)
    ;;         (mopr-uri:resource-id-query rid))
    (xmls:toxml
     (apply #'handle-get-request rid
            (dispatch-path (mopr-uri:resource-id-path rid) +dispatch-tree+)))))
