;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr
  (:import-from :mopr)
  (:import-from :plus-c) ; Depend on system.
  (:import-from :mopr-ext/repr-shared
                #:multiple-set-c-ref
                #:with-layout-settings)
  (:import-from :mopr-ext/repr-rdata)
  (:import-from :mopr-ext/repr-rnode)
  (:use :cl)
  (:export
   #:populate-command-queue
   #:destruct-command-queue
   #:populate-command-options
   #:destruct-command-options
   #:apply-command-option))

(in-package :mopr-ext/repr)

(defvar *debug-mode* t)

(defvar *enable-call* nil)

(defvar *root-rnode* nil)

;;
;;; Utilities
;;

(defun unknown-form-error (form action)
  (format t "
[ERROR] Cannot handle form.
[  -  ] FORM: ~A
[  -  ] ACTION: ~A
"
          form
          (case action
            (:skip "Skipping.")
            (:debug  (if *debug-mode*
                         "Debug mode enabled: Will error."
                         "Debug mode disabled: Skipping."))
            (:error  "Will error.")
            (otherwise (error "Coding error. Unknown message action."))))
  (when (and (eq action :debug) *debug-mode*)
    (error "Cannot handle form: ~S~%" form)))

;;
;;; Form Handlers
;;

(defun handle-var-form (rparent form)
  ;; (format t "~%Called handle-var-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:var-rnode :rparent rparent :form form)
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-each-form (rparent form)
  ;; (format t "~%Called handle-each-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:each-rnode :rparent rparent :form form)
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-iota-form (rparent form)
  ;; (format t "~%Called handle-iota-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:iota-rnode :rparent rparent :form form)
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-call-form (rparent form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:call-rnode :rparent rparent :form form)
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-prim-form (rparent form)
  ;; (format t "~%Called handle-prim-form!~%: ~S~%" form)
  (let* ((pn (make-instance 'mopr-ext/repr-rnode:prim-rnode :rparent rparent :form form)))
    (vector-push-extend pn (mopr-ext/repr-rnode:rnode-children rparent))
    (loop for l in (cddr form)
          for i from 0
          for fn = (case (car l)
                     ;; TODO : Handle other forms.
                     (:call   #'handle-call-form)
                     (:|call| #'handle-call-form))
          do (if fn
                 (funcall fn pn (cdr l))
                 (unknown-form-error (car l) :debug)))))

(defun handle-tree-form (rparent form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:tree-rnode :rparent rparent :form form)
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-meta-form (rparent form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)
  (vector-push-extend (make-instance 'mopr-ext/repr-rnode:meta-rnode :rparent rparent :form form)
                      (mopr-ext/repr-rnode:rnode-children rparent)))

(defun handle-data-subforms (rparent subforms)
  ;; (format t "~%Called handle-data-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for i from 0
        for fn = (case (car l)
                   (:var    #'handle-var-form)
                   (:|var|  #'handle-var-form)
                   (:each   #'handle-each-form)
                   (:|each| #'handle-each-form)
                   (:iota   #'handle-iota-form)
                   (:|iota| #'handle-iota-form)
                   (:call   #'handle-call-form)
                   (:|call| #'handle-call-form)
                   (:meta   #'handle-meta-form)
                   (:|meta| #'handle-meta-form)
                   (:tree   #'handle-tree-form)
                   (:|tree| #'handle-tree-form)
                   (:prim   #'handle-prim-form)
                   (:|prim| #'handle-prim-form))
        do (if fn
               (funcall fn rparent (cdr l))
               (unknown-form-error (car l) :debug))))

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

(defmacro with-repr-variables ((&key
                                  (enable-call nil))
                               &body body)
  `(let* ((*enable-call* ,enable-call))
     ,@body))

(defun build-repr-call-enabled (usds-data)
  (mopr-info:with-registry (:supported-cases '(:upcase))
    (mopr-plug:with-configuration ()
      (with-repr-variables (:enable-call t)
        (let* ((n (make-instance 'mopr-ext/repr-rnode:root-rnode)))
          (handle-data-subforms n usds-data) n)))))

(defun %populate-commands-recursive (n wcmds)
  (loop for rd in (mopr-ext/repr-rnode:rnode-rdatas n)
        unless (typep rd 'mopr-ext/repr-rdata:hidden-rdata)
          do (let ((cmd (cvec-get-incrementing-counter wcmds)))
               (mopr-ext/repr-rnode:populate-command-from-rnode n cmd)
               (mopr-ext/repr-rdata:populate-command-from-rdata rd cmd)))
  (loop for c across (mopr-ext/repr-rnode:rnode-children n)
        do (%populate-commands-recursive c wcmds)))

(defun %count-visible-rdata-recursive (n)
  (+ (count-if-not (lambda (x) (typep x 'mopr-ext/repr-rdata:hidden-rdata))
                   (mopr-ext/repr-rnode:rnode-rdatas n))
     (loop for c across (mopr-ext/repr-rnode:rnode-children n)
           summing (%count-visible-rdata-recursive c))))

(defun populate-command-queue (cmd-queue usds-data)
  (with-layout-settings
      ;; TODO : Implement cleanup.
      (setf *root-rnode* (build-repr-call-enabled usds-data))
    (let* ((pixels-w (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-w))
           ;; (pixels-h (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-h))
           (root-yn (mopr-ext/repr-rdata:rdata-ynode
                     (car (mopr-ext/repr-rnode:rnode-rdatas *root-rnode*))))
           (wcmds
             (make-instance 'cvec
                            :ctype 'mopr-def:combined-command
                            :size (%count-visible-rdata-recursive *root-rnode*))))

      (yoga-fun:node-calculate-layout root-yn
                                      pixels-w
                                      yoga-def:+undefined+ ;; pixels-h
                                      yoga-def:+direction-ltr+)

      (%populate-commands-recursive *root-rnode* wcmds)

      (multiple-set-c-ref cmd-queue (mopr-def:command-queue)
                          :nof-commands (cvec-size wcmds)
                          :commands (autowrap:ptr (cvec-wrapper wcmds))
                          ;; Adjust height to the actual "used" height.
                          :pixels-h (mopr-ext/repr-shared:layout-dimension root-yn :height))

      ;; TODO : Defer.
      (yoga-fun:node-free-recursive root-yn))))

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

    (multiple-set-c-ref cmd-queue (mopr-def:command-queue) :nof-commands 0 :commands (autowrap:ptr nil))))

(defun destruct-command-options (cmd-options-ptr
                                 &aux
                                   (cmd-options (autowrap:wrap-pointer
                                                 cmd-options-ptr 'mopr-def:command-options)))
  (let* ((opt-count (plus-c:c-ref cmd-options mopr-def:command-options :nof-options))
         (options (plus-c:c-ref cmd-options mopr-def:command-options :options)))

    (loop for i below opt-count
          do (autowrap:free (autowrap:c-aref options i :pointer)))

    (autowrap:free options)

    (multiple-set-c-ref cmd-options (mopr-def:command-options) :nof-options 0 :options (autowrap:ptr nil))))

(defun populate-command-options (cmd-options-ptr id id-sub
                                 &aux
                                   (cmd-options (autowrap:wrap-pointer
                                                 cmd-options-ptr 'mopr-def:command-options)))
  (let* ((n (mopr-ext/repr-rnode:find-rnode-by-id *root-rnode* id))
         (opts (mopr-ext/repr-rnode:get-rdata-options n id-sub))
         (nof-opts (length opts))
         (vopts (autowrap:alloc :pointer nof-opts)))

    (loop for o in opts for i from 0
          do (setf (autowrap:c-aref vopts i :pointer) (autowrap:alloc-string o)))

    (multiple-set-c-ref cmd-options (mopr-def:command-options)
                        :nof-options nof-opts
                        :options (autowrap:ptr vopts))))

(defun apply-command-option (id id-sub id-opt)
  (let* ((n (mopr-ext/repr-rnode:find-rnode-by-id *root-rnode* id))
         (opts (mopr-ext/repr-rnode:get-rdata-options n id-sub))
         (idx (1- id-opt)))
    (format t "APPLIED OPTION: ~A~%" (nth idx opts))))
