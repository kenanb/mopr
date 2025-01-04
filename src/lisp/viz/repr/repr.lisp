;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-viz/repr
  (:import-from :mopr-viz/repr-shared
                #:multiple-set-c-ref)
  (:use :cl)
  (:export
   #:%populate-command-queue
   #:%destruct-command-queue
   #:%apply-command-option))

(in-package :mopr-viz/repr)

;;
;;; Utilities
;;

(defun enode-rdatas (node)
  (let ((rn (mopr-sgt:enode-find-component node 'mopr-viz/repr-rnode:rnode)))
    (mopr-viz/repr-rnode:rnode-rdatas rn)))

;;
;;; Trivial Vector Type Backed by a C Array
;;

(defclass cvec ()
  ((ctype
    :type symbol
    :initarg :ctype
    :initform (error "Ctype must be specified!")
    :accessor cvec-ctype)
   (size
    :type (unsigned-byte 32)
    :initarg :size
    :initform 0
    :accessor cvec-size)
   (idx
    :type (unsigned-byte 32)
    :initform 0
    :accessor cvec-idx)
   (wrapper
    :accessor cvec-wrapper)))

(defmethod initialize-instance :after ((vec cvec) &key)
  (setf (cvec-wrapper vec)
        (autowrap:alloc (cvec-ctype vec) (cvec-size vec))))

(defun cvec-get-incrementing-counter (vec)
  (prog1
      (autowrap:c-aref (cvec-wrapper vec)
                       (cvec-idx vec)
                       (cvec-ctype vec))
    (incf (cvec-idx vec))))

;;
;;; Top-Level API and Macros
;;

(defun %populate-commands-recursive (n wcmds)
  (let ((c-id (mopr-sgt:enode-find-component n 'mopr-msg/ctrl:node-identifier))
        (c-rn (mopr-sgt:enode-find-component n 'mopr-viz/repr-rnode:rnode)))
    ;; (unless c-id (error "Component missing: MOPR-MSG/CTRL:NODE-IDENTIFIER"))
    ;; (unless c-rn (error "Component missing: MOPR-VIZ/REPR-RNODE:RNODE"))
    (loop with id = (mopr-msg/ctrl:node-identifier-id c-id)
          with id-sub-last of-type (unsigned-byte 32) = 0
          for rd in (mopr-viz/repr-rnode:rnode-rdatas c-rn)
          for id-sub = (if (typep rd 'mopr-viz/repr-rdata:frozen-rdata) 0 (incf id-sub-last))
          unless (typep rd 'mopr-viz/repr-rdata:hidden-rdata)
            do (let ((cmd (cvec-get-incrementing-counter wcmds)))
                 (mopr-viz/repr-rdata:populate-command-from-rdata rd cmd)
                 (multiple-set-c-ref cmd (mopr-viz/repr-def:combined-command :base)
                                     :id id
                                     :id-sub id-sub))))
  (loop for c across (mopr-sgt:enode-children n)
        do (%populate-commands-recursive c wcmds)))

(defun %count-visible-rdata-recursive (n)
  (+ (count-if-not (lambda (x) (typep x 'mopr-viz/repr-rdata:hidden-rdata))
                   (enode-rdatas n))
     (loop for c across (mopr-sgt:enode-children n)
           summing (%count-visible-rdata-recursive c))))

(defun %populate-command-queue (pr cmd-queue-ptr
                                &aux
                                  (cmd-queue (autowrap:wrap-pointer
                                              cmd-queue-ptr 'mopr-viz/repr-def:command-queue))
                                  (root-enode (mopr-sgt:procedure-root pr)))
  (mopr-viz/layout-shared:with-layout-settings
    (let* ((pixels-w (plus-c:c-ref cmd-queue mopr-viz/repr-def:command-queue :pixels-w))
           ;; (pixels-h (plus-c:c-ref cmd-queue mopr-viz/repr-def:command-queue :pixels-h))
           (root-yn (mopr-viz/repr-rdata:rdata-ynode
                     (car (enode-rdatas root-enode))))
           (wcmds
             (make-instance 'cvec
                            :ctype 'mopr-viz/repr-def:combined-command
                            :size (%count-visible-rdata-recursive root-enode))))

      (mopr-viz/yoga-fun:node-calculate-layout root-yn
                                               pixels-w
                                               mopr-viz/yoga-def:+undefined+ ;; pixels-h
                                               mopr-viz/yoga-def:+direction-ltr+)

      (%populate-commands-recursive root-enode wcmds)

      (multiple-set-c-ref cmd-queue (mopr-viz/repr-def:command-queue)
                          :nof-commands (cvec-size wcmds)
                          :commands (autowrap:ptr (cvec-wrapper wcmds))
                          ;; Adjust height to the actual "used" height.
                          :pixels-h (mopr-viz/layout-shared:layout-dimension root-yn :height)))))

;; NOTE: Free calls are made from the same module the allocations were made from,
;;       to avoid possible issues with multiple malloc implementations in runtime.
(defun %destruct-command-queue (cmd-queue-ptr
                                &aux
                                  (cmd-queue (autowrap:wrap-pointer
                                              cmd-queue-ptr 'mopr-viz/repr-def:command-queue)))
  (let* ((cmd-count (plus-c:c-ref cmd-queue mopr-viz/repr-def:command-queue :nof-commands))
         (commands (plus-c:c-ref cmd-queue mopr-viz/repr-def:command-queue :commands)))

    (loop for i below cmd-count
          for c = (autowrap:c-aref commands i 'mopr-viz/repr-def:combined-command)
          for c-type = (plus-c:c-ref c mopr-viz/repr-def:combined-command :base :c-type)

          ;; TODO: Formalize cleanup.
          ;;
          ;; NOTE: c-ref implements some convenience functionality based on the
          ;; last field.  However, in order to free the C string, we do need the
          ;; pointer.  So we dereference and reference in the end, to inhibit
          ;; foreign-string last-field convenience.
          do (case c-type
               (mopr-viz/repr-def:+command-type-draw-expr-label+
                (autowrap:free (plus-c:c-ref c mopr-viz/repr-def:combined-command
                                             :draw-expr-label
                                             :text plus-c:* plus-c:&)))
               (mopr-viz/repr-def:+command-type-draw-attr-label+
                (autowrap:free (plus-c:c-ref c mopr-viz/repr-def:combined-command
                                             :draw-attr-label
                                             :text plus-c:* plus-c:&)))
               (mopr-viz/repr-def:+command-type-draw-attr-input+
                (autowrap:free (plus-c:c-ref c mopr-viz/repr-def:combined-command
                                             :draw-attr-input
                                             :text plus-c:* plus-c:&)))))

    (autowrap:free commands)

    (multiple-set-c-ref cmd-queue (mopr-viz/repr-def:command-queue) :nof-commands 0
                                                                    :commands (autowrap:ptr nil))))

(defun %apply-command-option (pr id id-sub id-opt)
  (when (zerop id-sub) (error "Zero id-sub passed to root-enode-apply-command-option!"))
  (when (zerop id-opt) (error "Zero id-opt passed to root-enode-apply-command-option!"))
  (mopr-sgt:with-bound-procedure-accessors ((root mopr-sgt:procedure-root)) pr
    (let* ((n (mopr-msg/ctrl:find-enode-by-id root id))
           (opts (mopr-msg/ctrl:payload-get-options (mopr-sgt:bnode-find-payload n) (1- id-sub)))
           (idx (1- id-opt)))
      (format t "APPLIED OPTION: ~A~%" (nth idx opts)))))
