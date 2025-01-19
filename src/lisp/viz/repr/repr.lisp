;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-viz/repr
  (:use :cl)
  (:export
   #:calculate-editor-layout))

(in-package :mopr-viz/repr)

(defmethod mopr-ops:get-info-for-component ((co mopr-viz/repr-rnode:rnode))
  (loop with id-sub-last of-type (unsigned-byte 32) = 0
        for rd in (mopr-viz/repr-rnode:rnode-rdatas co)
        for id-sub = (if (typep rd 'mopr-viz/repr-rdata:frozen-rdata) 0 (incf id-sub-last))
        unless (typep rd 'mopr-viz/repr-rdata:hidden-rdata)
          collect `(("id-sub" ,id-sub)
                    ,@(mopr-viz/repr-rdata:rdata-command-attributes rd))))

(defun calculate-editor-layout (root-enode pixels-w pixels-h)
  (declare (ignore pixels-h))
  (mopr-viz/layout-shared:with-layout-settings
      (let* ((rn (mopr-sgt:enode-find-component root-enode 'mopr-viz/repr-rnode:rnode))
             (root-yn (mopr-viz/repr-rdata:rdata-ynode
                       (car (mopr-viz/repr-rnode:rnode-rdatas rn)))))
        (mopr-viz/yoga-fun:node-calculate-layout root-yn
                                                 (float pixels-w)
                                                 mopr-viz/yoga-def:+undefined+ ;; pixels-h
                                                 mopr-viz/yoga-def:+direction-ltr+)
        (cons
         `(("pixels-w" ,(write-to-string pixels-w))
           ("pixels-h" ,(write-to-string (mopr-viz/layout-shared:layout-dimension
                                          root-yn :height))))
         (mopr-ops:collect-info-for-components root-enode '(mopr-viz/repr-rnode:rnode))))))
