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

  (:method (rid (category (eql 'request-fn-unknown))
            &key context remaining)
    (failure-response :get rid category context remaining "unsupported")))

(defgeneric handle-post-request (rid request-body category &key context remaining)

  (:method (rid request-body category &key context remaining)
    (format t "POST REQUEST BODY:~%~A~%" request-body)
    (failure-response :post rid category context remaining "unimplemented"))

  (:method (rid request-body (category (eql 'base-request-fn-top))
            &key context remaining)
    (declare (ignore rid request-body category context remaining))
    (error "The POST request handle BASE-REQUEST-FN-TOP should be unreachable with the current design!"))

  (:method (rid request-body (category (eql 'request-fn-unknown))
            &key context remaining)
    (format t "POST REQUEST BODY:~%~A~%" request-body)
    (failure-response :post rid category context remaining "unsupported")))
