;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-msg)

(defmethod handle-get-request (rid (category (eql 'request-fn-relative))
                               &key context remaining)
  (declare (ignore rid category context remaining))
  (error "The GET request handle REQUEST-FN-RELATIVE is currently unsupported!"))

(defmethod handle-post-request (rid request-body (category (eql 'request-fn-relative))
                                &key context remaining)
  (declare (ignore rid request-body category context remaining))
  (error "The POST request handle REQUEST-FN-RELATIVE is currently unsupported!"))
