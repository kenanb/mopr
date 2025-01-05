;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defun dispatch-path (keyform-list handler)
  "DISPATCH-PATH keyform-list handler => (category-symbol :context plist :remaining list)
handler::= { ancestral-handler | innermost-handler }
ancestral-handler::= (context-parameter category-symbol clause*)
innermost-handler::= { (context-parameter category-symbol) | category-symbol }
clause::= (key . handler)
key ::= { t | match-string }

Atomic form of innermost-handler is syntactic sugar for the list form (:THIS category-symbol).
This allows visually treating dot as a placeholder for :THIS in innermost-handler.
"
  (loop for ht = handler
          then (loop for (m . hn) in dp if (or (eq m t) (equal m v)) return hn)
        for (k dpm . dp) = (if (consp ht) ht (list :this ht))
        for (v . r) on keyform-list appending (list k v) into context while dp
        finally (return (list dpm :context context :remaining r))))

(defconstant +dispatch-tree-working+
  `(;; URI PREFIX: /workshop/wID/project/pID/asset/aID/working/DIGEST/
    :working-res-ep base-request-fn-working-res
    ("" . main-request-fn-working-res)
    ("option" . base-request-fn-option)
    ("editor-layout" . base-request-fn-editor-layout)
    (t . request-fn-unknown)))

(defconstant +dispatch-tree-asset+
  `(;; URI PREFIX: /workshop/wID/project/pID/asset/aID/
    :asset-res-ep base-request-fn-asset-res
    ("" . main-request-fn-asset-res)
    ("staging" . base-request-fn-staging)
    ("working"
     :working-res base-request-fn-working-ep
     ("" . main-request-fn-working-ep)
     (t . ,+dispatch-tree-working+))
    (t . request-fn-unknown)))

(defconstant +dispatch-tree-project+
  `(;; URI PREFIX: /workshop/wID/project/pID/
    :project-res-ep base-request-fn-project-res
    ("" . main-request-fn-project-res)
    ("lock" . main-request-fn-project-lock-ep)
    ("asset"
     :asset-res base-request-fn-asset-ep
     ("" . main-request-fn-asset-ep)
     (t . ,+dispatch-tree-asset+))
    (t . request-fn-unknown)))

(defconstant +dispatch-tree-workshop+
  `(;; URI PREFIX: /workshop/wID/
    :workshop-res-ep base-request-fn-workshop-res
    ("" . main-request-fn-workshop-res)
    ("project"
     :project-res base-request-fn-project-ep
     ("" . main-request-fn-project-ep)
     (t . ,+dispatch-tree-project+))
    (t . request-fn-unknown)))

(defconstant +dispatch-tree-absolute+
  `(;; URI PREFIX: /
    :absolute-ep base-request-fn-absolute-ep
    ("" . main-request-fn-absolute-ep)
    ("workshop"
     :workshop-res base-request-fn-workshop-ep
     ("" . main-request-fn-workshop-ep)
     (t . ,+dispatch-tree-workshop+))
    (t . request-fn-unknown)))

(defconstant +dispatch-tree+
  `(;; The root of the tree is a handler definition, and not a clause. Because
    ;; the root handler is assumed to be the CDR of an unconditional match
    ;; clause T.
    :top base-request-fn-top
    ("" . ,+dispatch-tree-absolute+)
    (t . request-fn-relative)))
