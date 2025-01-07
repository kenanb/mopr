;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-viz/repr
  (:use :cl)
  (:export
   #:calculate-editor-layout))

(in-package :mopr-viz/repr)

;;
;;; Utilities
;;

(defun enode-rdatas (node)
  (let ((rn (mopr-sgt:enode-find-component node 'mopr-viz/repr-rnode:rnode)))
    (mopr-viz/repr-rnode:rnode-rdatas rn)))

;;
;;; Top-Level API and Macros
;;

(defun %layout-elements-recursive (n)
  (let ((c-id (mopr-sgt:enode-find-component n 'mopr-msg/ctrl:node-identifier))
        (c-rn (mopr-sgt:enode-find-component n 'mopr-viz/repr-rnode:rnode)))
    ;; (unless c-id (error "Component missing: MOPR-MSG/CTRL:NODE-IDENTIFIER"))
    ;; (unless c-rn (error "Component missing: MOPR-VIZ/REPR-RNODE:RNODE"))
    (append
     (loop with id-node = (mopr-msg/ctrl:node-identifier-id c-id)
           with id-sub-last of-type (unsigned-byte 32) = 0
           for rd in (mopr-viz/repr-rnode:rnode-rdatas c-rn)
           for id-sub = (if (typep rd 'mopr-viz/repr-rdata:frozen-rdata) 0 (incf id-sub-last))
           unless (typep rd 'mopr-viz/repr-rdata:hidden-rdata)
             collect `(("id-node" ,id-node)
                       ("id-sub" ,id-sub)
                       ,@(mopr-viz/repr-rdata:rdata-command-attributes rd)))
     (loop for c across (mopr-sgt:enode-children n)
           appending (%layout-elements-recursive c)))))

(defun calculate-editor-layout (root-enode pixels-w pixels-h)
  (declare (ignore pixels-h))
  (mopr-viz/layout-shared:with-layout-settings
      (let* ((root-yn (mopr-viz/repr-rdata:rdata-ynode
                       (car (enode-rdatas root-enode)))))

        (mopr-viz/yoga-fun:node-calculate-layout root-yn
                                                 (float pixels-w)
                                                 mopr-viz/yoga-def:+undefined+ ;; pixels-h
                                                 mopr-viz/yoga-def:+direction-ltr+)

        (cons
         `(("pixels-w" ,(write-to-string pixels-w))
           ("pixels-h" ,(write-to-string (mopr-viz/layout-shared:layout-dimension
                                          root-yn :height))))
         (%layout-elements-recursive root-enode)))))
