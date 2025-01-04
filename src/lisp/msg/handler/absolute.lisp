;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defmethod handle-get-request (rid (category (eql 'main-request-fn-absolute-ep))
                               &key context remaining)
  (declare (ignore rid context remaining))
  (xmls:make-node
   :name "endpoints"
   :children
   (list (xmls:make-node :name "endpoint" :attrs `(("name" "workshop")
                                                   ("uri" "/workshop/"))))))
