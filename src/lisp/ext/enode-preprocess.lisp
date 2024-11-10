;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/enode-preprocess
  (:import-from :mopr)
  (:import-from :mopr-ext/enode-copy)
  (:use :mopr-sgt)
  (:use :cl)
  (:export

   #:preprocess-all
   #:preprocess-all-call-enabled

   ))

(in-package :mopr-ext/enode-preprocess)

;;
;;; MAIN API
;;

(defvar *enable-call* nil)
(defvar *var-table* nil)
(defvar *each-table* nil)

(defmacro with-expansion-variables ((&key
                                       (enable-call nil))
                                    &body body)
  `(let* ((*enable-call* ,enable-call)
          (*var-table* (make-hash-table))
          (*each-table* (make-hash-table)))
     ,@body))

(defgeneric preprocess (node)
  (:documentation "Preprocess enode."))

(defun preprocess-recursive (node &optional parent
                             &aux (preprocessed (preprocess node)))
  "Create a preprocessed enode hierarchy."
  (loop for e in preprocessed
        do (progn
             (when parent (vector-push-extend e (enode-children parent)))
             (loop for c across (enode-children node) do (preprocess-recursive c e))))
  preprocessed)

;; Assumes being called within a with-registry scope.
(defun preprocess-all (rn)
  (mopr-plug:with-configuration ()
    (with-expansion-variables (:enable-call nil)
      (car (preprocess-recursive rn)))))

(defun preprocess-all-call-enabled (rn)
  (mopr-plug:with-configuration ()
    (with-expansion-variables (:enable-call t)
      (car (preprocess-recursive rn)))))

;;
;;; PREPROCESS IMPLEMENTATIONS
;;

(defvar *debug-mode* t)

(defun validate-call-support (node action)
  (unless *enable-call*
    (format t "
[ERROR] Cannot handle node.
[  -  ] NODE: ~A
[  -  ] ACTION: ~A
"
            node
            (case action
              (:skip "Skipping.")
              (:debug  (if *debug-mode*
                           "Debug mode enabled: Will error."
                           "Debug mode disabled: Skipping."))
              (:error  "Will error.")
              (otherwise (error "Coding error. Unknown message action."))))
    (when (and (eq action :debug) *debug-mode*)
      (error "Cannot handle node: ~S~%" node))))

(defmethod preprocess ((node enode))
  (list (mopr-ext/enode-copy:copy-enode-instance node)))

(defmethod preprocess ((node var-enode))
  (validate-call-support node :debug)
  (with-accessors ((name var-enode-name-param)
                   (aux var-enode-aux-form-param)
                   (val var-enode-val-form-param)) node
    (setf (gethash name *var-table*)
          (car (mopr-plug:process-call-stack aux val *var-table*))))
  nil)

(defmethod preprocess ((node each-enode))
  (validate-call-support node :debug)
  (with-accessors ((name each-enode-name-param)
                   (keys each-enode-keys-form-param)
                   (vals each-enode-vals-form-param)) node
    (setf (gethash name *each-table*)
          (mapcar (if (listp keys)
                      (lambda (args) (mapcan #'list keys args))
                      (lambda (args) (list keys args)))
                  vals)))
  nil)

(defmethod preprocess ((node iota-enode))
  (validate-call-support node :debug)
  (with-accessors ((name iota-enode-name-param)
                   (key iota-enode-key-param)
                   (end iota-enode-end-param)
                   (start iota-enode-start-param)
                   (step iota-enode-step-param)) node
    (setf (gethash name *each-table*)
          (mapcar (lambda (args) (list key args))
                  (alexandria:iota end
                                   :start (or start 0)
                                   :step (or step 1)))))
  nil)

(defun process-and-filter-call-stack (args body-form)
  (remove-if-not
   (lambda (x) (typep x 'enode))
   (mopr-plug:process-call-stack args body-form *var-table*)))

;; TODO: Reapply expansion to results.
(defun preprocess-call-generic (node)
  (validate-call-support node :debug)
  (with-accessors ((aux-form call-enode-aux-form-param)
                   (body-form call-enode-body-form-param)) node
    (let ((args-list (if (listp aux-form)
                         (list aux-form)
                         (gethash aux-form *each-table*))))
      (loop for args in args-list
            nconc (process-and-filter-call-stack args body-form)))))

(defmethod preprocess ((node prim-call-enode))
  (preprocess-call-generic node))

(defmethod preprocess ((node call-enode))
  (preprocess-call-generic node))
