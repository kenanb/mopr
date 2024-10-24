;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr
  (:import-from :mopr)
  (:import-from :plus-c) ; Depend on system.
  (:import-from :mopr-ext/repr-shared
                #:multiple-set-c-ref
                #:with-layout-settings)
  (:import-from :mopr-ext/enode)
  (:import-from :mopr-ext/serialization)
  (:import-from :mopr-ext/repr-rdata)
  (:import-from :mopr-ext/repr-rnode)
  (:use :cl)
  (:export
   #:create-enode-tree
   #:delete-enode-tree
   #:populate-command-queue
   #:destruct-command-queue
   #:populate-command-options
   #:destruct-command-options
   #:apply-command-option))

(in-package :mopr-ext/repr)

(defvar *root-enode* nil)

;;
;;; ENODE Tree
;;

(defun create-enode-tree (usds-data)
  (setf *root-enode* (mopr-ext/serialization:deserialize-call-enabled usds-data)))

(defun delete-enode-tree ()
  (yoga-fun:node-free-recursive (mopr-ext/repr-rdata:rdata-ynode
                                 (car (mopr-ext/repr-rnode:enode-rdatas *root-enode*)))))

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
  (loop for rd in (mopr-ext/repr-rnode:enode-rdatas n)
        unless (typep rd 'mopr-ext/repr-rdata:hidden-rdata)
          do (let ((cmd (cvec-get-incrementing-counter wcmds)))
               (mopr-ext/repr-rnode:populate-command-from-enode n cmd)
               (mopr-ext/repr-rdata:populate-command-from-rdata rd cmd)))
  (loop for c across (mopr-ext/enode:enode-children n)
        do (%populate-commands-recursive c wcmds)))

(defun %count-visible-rdata-recursive (n)
  (+ (count-if-not (lambda (x) (typep x 'mopr-ext/repr-rdata:hidden-rdata))
                   (mopr-ext/repr-rnode:enode-rdatas n))
     (loop for c across (mopr-ext/enode:enode-children n)
           summing (%count-visible-rdata-recursive c))))

(defun populate-command-queue (cmd-queue)
  (with-layout-settings
      (let* ((pixels-w (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-w))
             ;; (pixels-h (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-h))
             (root-yn (mopr-ext/repr-rdata:rdata-ynode
                       (car (mopr-ext/repr-rnode:enode-rdatas *root-enode*))))
             (wcmds
               (make-instance 'cvec
                              :ctype 'mopr-def:combined-command
                              :size (%count-visible-rdata-recursive *root-enode*))))

        (yoga-fun:node-calculate-layout root-yn
                                        pixels-w
                                        yoga-def:+undefined+ ;; pixels-h
                                        yoga-def:+direction-ltr+)

        (%populate-commands-recursive *root-enode* wcmds)

        (multiple-set-c-ref cmd-queue (mopr-def:command-queue)
                            :nof-commands (cvec-size wcmds)
                            :commands (autowrap:ptr (cvec-wrapper wcmds))
                            ;; Adjust height to the actual "used" height.
                            :pixels-h (mopr-ext/repr-shared:layout-dimension root-yn :height)))))

;; NOTE: Free calls are made from the same module the allocations were made from,
;;       to avoid possible issues with multiple malloc implementations in runtime.
(defun destruct-command-queue (cmd-queue-ptr
                               &aux
                                 (cmd-queue (autowrap:wrap-pointer
                                             cmd-queue-ptr 'mopr-def:command-queue)))
  (let* ((cmd-count (plus-c:c-ref cmd-queue mopr-def:command-queue :nof-commands))
         (commands (plus-c:c-ref cmd-queue mopr-def:command-queue :commands)))

    (loop for i below cmd-count
          for c = (autowrap:c-aref commands i 'mopr-def:combined-command)
          for c-type = (plus-c:c-ref c mopr-def:combined-command :base :c-type)

          ;; TODO: Formalize cleanup.
          ;;
          ;; NOTE: c-ref implements some convenience functionality based on the
          ;; last field.  However, in order to free the C string, we do need the
          ;; pointer.  So we dereference and reference in the end, to inhibit
          ;; foreign-string last-field convenience.
          do (case c-type
               (mopr-def:+command-type-draw-expr-label+
                (autowrap:free (plus-c:c-ref c mopr-def:combined-command
                                             :draw-expr-label
                                             :text plus-c:* plus-c:&)))
               (mopr-def:+command-type-draw-attr-label+
                (autowrap:free (plus-c:c-ref c mopr-def:combined-command
                                             :draw-attr-label
                                             :text plus-c:* plus-c:&)))
               (mopr-def:+command-type-draw-attr-input+
                (autowrap:free (plus-c:c-ref c mopr-def:combined-command
                                             :draw-attr-input
                                             :text plus-c:* plus-c:&)))))

    (autowrap:free commands)

    (multiple-set-c-ref cmd-queue (mopr-def:command-queue) :nof-commands 0
                                                           :commands (autowrap:ptr nil))))

(defun destruct-command-options (cmd-options-ptr
                                 &aux
                                   (cmd-options (autowrap:wrap-pointer
                                                 cmd-options-ptr 'mopr-def:command-options)))
  (let* ((opt-count (plus-c:c-ref cmd-options mopr-def:command-options :nof-options))
         (options (plus-c:c-ref cmd-options mopr-def:command-options :options)))

    (loop for i below opt-count
          do (autowrap:free (autowrap:c-aref options i :pointer)))

    (autowrap:free options)

    (multiple-set-c-ref cmd-options (mopr-def:command-options) :nof-options 0
                                                               :options (autowrap:ptr nil))))

(defun populate-command-options (cmd-options-ptr id id-sub
                                 &aux
                                   (cmd-options (autowrap:wrap-pointer
                                                 cmd-options-ptr 'mopr-def:command-options)))
  (let* ((n (mopr-ext/repr-rnode:find-enode-by-id *root-enode* id))
         (opts (mopr-ext/repr-rnode:enode-get-rdata-options n id-sub))
         (nof-opts (length opts))
         (vopts (autowrap:alloc :pointer nof-opts)))

    (loop for o in opts for i from 0
          do (setf (autowrap:c-aref vopts i :pointer) (autowrap:alloc-string o)))

    (multiple-set-c-ref cmd-options (mopr-def:command-options)
                        :nof-options nof-opts
                        :options (autowrap:ptr vopts))))

(defun apply-command-option (id id-sub id-opt)
  (let* ((n (mopr-ext/repr-rnode:find-enode-by-id *root-enode* id))
         (opts (mopr-ext/repr-rnode:enode-get-rdata-options n id-sub))
         (idx (1- id-opt)))
    (format t "APPLIED OPTION: ~A~%" (nth idx opts))))
