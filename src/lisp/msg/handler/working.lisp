;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defmethod handle-get-request (rid (category (eql 'base-request-fn-editor-layout))
                               &key context remaining)
  (declare (ignore category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (messaging-session-puuid *messaging-session*))
        (auuid (getf context :asset-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (unless (equal puuid (getf context :project-res))
      (error "Project ID doesn't match the active project."))
    (let ((result (apply #'pr-populate-editor-layout (mopr-uri:resource-id-query rid))))
      (xmls:make-node
       :name "layout"
       :attrs (car result)
       :children (loop for o in (cdr result)
                       collecting (xmls:make-node :name "command" :attrs o))))))

(defmethod handle-get-request (rid (category (eql 'base-request-fn-option))
                               &key context remaining)
  (declare (ignore category remaining))
  (let ((wuuid (mopr-uri:descriptor-uuid (ws-descriptor)))
        (puuid (messaging-session-puuid *messaging-session*))
        (auuid (getf context :asset-res)))
    (unless (equal wuuid (getf context :workshop-res))
      (error "Workshop ID doesn't match the active workshop."))
    (unless (equal puuid (getf context :project-res))
      (error "Project ID doesn't match the active project."))
    (let ((result (apply #'pr-populate-command-options (mopr-uri:resource-id-query rid))))
      (xmls:make-node
       :name "options"
       :attrs (car result)
       :children (loop for o in (cdr result)
                       collecting (xmls:make-node :name "option" :attrs o))))))

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
           ((equal action-name "init-interaction")
            (pr-init-interaction)
            (format t "Representation components for bound procedure are initialized.~%"))
           ((equal action-name "term-interaction")
            (pr-term-interaction)
            (format t "Representation components for bound procedure are terminated.~%"))))))
    ;; TODO : Revisit.
    (xmls:make-node :name "action-result" :attrs `(("status" "OK")))))

(defmethod handle-post-request (rid request-body (category (eql 'base-request-fn-option))
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
           ((equal action-name "apply-option")
            (let ((id-node (parse-integer (cadr (assoc "id-node" (xmls:node-attrs request-body) :test #'string=))))
                  (id-sub (parse-integer (cadr (assoc "id-sub" (xmls:node-attrs request-body) :test #'string=))))
                  (id-opt (parse-integer (cadr (assoc "id-opt" (xmls:node-attrs request-body) :test #'string=)))))
              (pr-apply-command-option id-node id-sub id-opt)))))))
    ;; TODO : Revisit.
    (xmls:make-node :name "action-result" :attrs `(("status" "OK")))))
