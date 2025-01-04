;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

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
    (populate-command-options (mopr-uri:resource-id-query rid))))

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
