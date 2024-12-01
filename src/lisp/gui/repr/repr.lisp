;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-gui/repr
  (:import-from :mopr-gui/repr-shared
                #:multiple-set-c-ref)
  (:use :cl)
  (:export
   #:bind-for-representation
   #:initialize-repr
   #:deinitialize-repr
   #:populate-command-queue
   #:destruct-command-queue
   #:populate-command-options
   #:destruct-command-options
   #:apply-command-option))

(in-package :mopr-gui/repr)

(defvar *procedure* nil)

;;
;;; Utilities
;;

(defun enode-rdatas (node)
  (let ((rn (mopr-sgt:enode-find-component node 'mopr-gui/repr-rnode:rnode)))
    (mopr-gui/repr-rnode:rnode-rdatas rn)))

(defun get-root-enode () (mopr-sgt:procedure-root *procedure*))

;;
;;; ENODE Tree
;;

(defun bind-for-representation (pr)
  (setf *procedure* (mopr-sgt:make-enode-procedure pr)))

(defun initialize-repr ()
  (mopr-sgt:enode-add-components-recursive (get-root-enode) '(mopr-gui/repr-rnode:rnode))
  (mopr-gui/layout-shared:with-layout-settings
      (mopr-sgt:enode-initialize-components-recursive (get-root-enode))))

(defun deinitialize-repr ()
  (mopr-gui/yoga-fun:node-free-recursive (mopr-gui/repr-rdata:rdata-ynode
                                          (car (enode-rdatas (get-root-enode))))))

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

(defun %populate-commands-recursive (n wcmds
                                     &aux
                                       (rn (mopr-sgt:enode-find-component
                                            n
                                            'mopr-gui/repr-rnode:rnode)))
  (loop for rd in (mopr-gui/repr-rnode:rnode-rdatas rn)
        unless (typep rd 'mopr-gui/repr-rdata:hidden-rdata)
          do (let ((cmd (cvec-get-incrementing-counter wcmds)))
               (mopr-gui/repr-rnode:populate-command-from-rnode rn cmd)
               (mopr-gui/repr-rdata:populate-command-from-rdata rd cmd)))
  (loop for c across (mopr-sgt:enode-children n)
        do (%populate-commands-recursive c wcmds)))

(defun %count-visible-rdata-recursive (n)
  (+ (count-if-not (lambda (x) (typep x 'mopr-gui/repr-rdata:hidden-rdata))
                   (enode-rdatas n))
     (loop for c across (mopr-sgt:enode-children n)
           summing (%count-visible-rdata-recursive c))))

(defun populate-command-queue (cmd-queue-ptr
                               &aux
                                 (cmd-queue (autowrap:wrap-pointer
                                             cmd-queue-ptr 'mopr-gui/repr-def:command-queue)))
  (mopr-gui/layout-shared:with-layout-settings
      (let* ((pixels-w (plus-c:c-ref cmd-queue mopr-gui/repr-def:command-queue :pixels-w))
             ;; (pixels-h (plus-c:c-ref cmd-queue mopr-gui/repr-def:command-queue :pixels-h))
             (root-yn (mopr-gui/repr-rdata:rdata-ynode
                       (car (enode-rdatas (get-root-enode)))))
             (wcmds
               (make-instance 'cvec
                              :ctype 'mopr-gui/repr-def:combined-command
                              :size (%count-visible-rdata-recursive (get-root-enode)))))

        (mopr-gui/yoga-fun:node-calculate-layout root-yn
                                                 pixels-w
                                                 mopr-gui/yoga-def:+undefined+ ;; pixels-h
                                                 mopr-gui/yoga-def:+direction-ltr+)

        (%populate-commands-recursive (get-root-enode) wcmds)

        (multiple-set-c-ref cmd-queue (mopr-gui/repr-def:command-queue)
                            :nof-commands (cvec-size wcmds)
                            :commands (autowrap:ptr (cvec-wrapper wcmds))
                            ;; Adjust height to the actual "used" height.
                            :pixels-h (mopr-gui/layout-shared:layout-dimension root-yn :height)))))

;; NOTE: Free calls are made from the same module the allocations were made from,
;;       to avoid possible issues with multiple malloc implementations in runtime.
(defun destruct-command-queue (cmd-queue-ptr
                               &aux
                                 (cmd-queue (autowrap:wrap-pointer
                                             cmd-queue-ptr 'mopr-gui/repr-def:command-queue)))
  (let* ((cmd-count (plus-c:c-ref cmd-queue mopr-gui/repr-def:command-queue :nof-commands))
         (commands (plus-c:c-ref cmd-queue mopr-gui/repr-def:command-queue :commands)))

    (loop for i below cmd-count
          for c = (autowrap:c-aref commands i 'mopr-gui/repr-def:combined-command)
          for c-type = (plus-c:c-ref c mopr-gui/repr-def:combined-command :base :c-type)

          ;; TODO: Formalize cleanup.
          ;;
          ;; NOTE: c-ref implements some convenience functionality based on the
          ;; last field.  However, in order to free the C string, we do need the
          ;; pointer.  So we dereference and reference in the end, to inhibit
          ;; foreign-string last-field convenience.
          do (case c-type
               (mopr-gui/repr-def:+command-type-draw-expr-label+
                (autowrap:free (plus-c:c-ref c mopr-gui/repr-def:combined-command
                                             :draw-expr-label
                                             :text plus-c:* plus-c:&)))
               (mopr-gui/repr-def:+command-type-draw-attr-label+
                (autowrap:free (plus-c:c-ref c mopr-gui/repr-def:combined-command
                                             :draw-attr-label
                                             :text plus-c:* plus-c:&)))
               (mopr-gui/repr-def:+command-type-draw-attr-input+
                (autowrap:free (plus-c:c-ref c mopr-gui/repr-def:combined-command
                                             :draw-attr-input
                                             :text plus-c:* plus-c:&)))))

    (autowrap:free commands)

    (multiple-set-c-ref cmd-queue (mopr-gui/repr-def:command-queue) :nof-commands 0
                                                                    :commands (autowrap:ptr nil))))

(defun destruct-command-options (cmd-options-ptr
                                 &aux
                                   (cmd-options (autowrap:wrap-pointer
                                                 cmd-options-ptr 'mopr-gui/repr-def:command-options)))
  (let* ((opt-count (plus-c:c-ref cmd-options mopr-gui/repr-def:command-options :nof-options))
         (options (plus-c:c-ref cmd-options mopr-gui/repr-def:command-options :options)))

    (loop for i below opt-count
          do (autowrap:free (autowrap:c-aref options i :pointer)))

    (autowrap:free options)

    (multiple-set-c-ref cmd-options (mopr-gui/repr-def:command-options) :nof-options 0
                                                                        :options (autowrap:ptr nil))))

(defun populate-command-options (cmd-options-ptr id id-sub
                                 &aux
                                   (cmd-options (autowrap:wrap-pointer
                                                 cmd-options-ptr 'mopr-gui/repr-def:command-options)))
  (let* ((n (mopr-gui/repr-rnode:find-enode-by-rnode-id (get-root-enode) id))
         (opts (mopr-gui/repr-rnode:payload-get-rdata-options (mopr-sgt:enode-payload n) id-sub))
         (nof-opts (length opts))
         (vopts (autowrap:alloc :pointer nof-opts)))

    (loop for o in opts for i from 0
          do (setf (autowrap:c-aref vopts i :pointer) (autowrap:alloc-string o)))

    (multiple-set-c-ref cmd-options (mopr-gui/repr-def:command-options)
                        :nof-options nof-opts
                        :options (autowrap:ptr vopts))))

(defun apply-command-option (id id-sub id-opt)
  (let* ((n (mopr-gui/repr-rnode:find-enode-by-rnode-id (get-root-enode) id))
         (opts (mopr-gui/repr-rnode:payload-get-rdata-options (mopr-sgt:enode-payload n) id-sub))
         (idx (1- id-opt)))
    (format t "APPLIED OPTION: ~A~%" (nth idx opts))))
