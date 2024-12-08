;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

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

(defgeneric preprocess (payload)
  (:documentation "Preprocess payload."))

(defun cnode-preprocess (node &aux (p (cnode-find-payload node)))
  (etypecase p
    (directive (preprocess p))
    (payload (list (make-cnode :payload p)))))

(defun preprocess-recursive (node &optional parent
                             &aux (preprocessed (cnode-preprocess node)))
  "Create a preprocessed cnode hierarchy."
  (loop for e in preprocessed
        do (progn
             (when parent (vector-push-extend e (cnode-children parent)))
             (loop for c across (cnode-children node) do (preprocess-recursive c e))))
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

(defun validate-call-support (payload action)
  (unless *enable-call*
    (format t "
[ERROR] Cannot handle payload.
[  -  ] PAYLOAD : ~A
[  -  ] ACTION  : ~A
"
            payload
            (case action
              (:skip "Skipping.")
              (:debug  (if *debug-mode*
                           "Debug mode enabled: Will error."
                           "Debug mode disabled: Skipping."))
              (:error  "Will error.")
              (otherwise (error "Coding error. Unknown message action."))))
    (when (and (eq action :debug) *debug-mode*)
      (error "Cannot handle payload: ~S~%" payload))))

(defmethod preprocess ((payload var-directive))
  (validate-call-support payload :debug)
  (with-accessors ((name var-directive-name-param)
                   (aux var-directive-aux-form-param)
                   (val var-directive-val-form-param)) payload
    (setf (gethash name *var-table*)
          (car (mopr-plug:process-call-stack aux val *var-table*))))
  nil)

(defmethod preprocess ((payload each-directive))
  (validate-call-support payload :debug)
  (with-accessors ((name each-directive-name-param)
                   (keys each-directive-keys-form-param)
                   (vals each-directive-vals-form-param)) payload
    (setf (gethash name *each-table*)
          (mapcar (if (listp keys)
                      (lambda (args) (mapcan #'list keys args))
                      (lambda (args) (list keys args)))
                  vals)))
  nil)

(defmethod preprocess ((payload iota-directive))
  (validate-call-support payload :debug)
  (with-accessors ((name iota-directive-name-param)
                   (key iota-directive-key-param)
                   (end iota-directive-end-param)
                   (start iota-directive-start-param)
                   (step iota-directive-step-param)) payload
    (setf (gethash name *each-table*)
          (mapcar (lambda (args) (list key args))
                  (alexandria:iota end
                                   :start (or start 0)
                                   :step (or step 1)))))
  nil)

(defun has-directives-recursive (node)
  (if (typep (cnode-find-payload node) 'directive)
      t
      (some #'has-directives-recursive (cnode-children node))))

(defun process-and-filter-call-stack (args body-form)
  (let ((initial-result
          (remove-if-not
           (lambda (x) (typep x 'cnode))
           (mopr-plug:process-call-stack args body-form *var-table*))))
    (if (some #'has-directives-recursive initial-result)
        (loop for n in initial-result nconc (preprocess-recursive n))
        initial-result)))

(defun preprocess-call-generic (payload)
  (validate-call-support payload :debug)
  (with-accessors ((aux-form call-directive-aux-form-param)
                   (body-form call-directive-body-form-param)) payload
    (let ((args-list (if (listp aux-form)
                         (list aux-form)
                         (gethash aux-form *each-table*))))
      (loop for args in args-list
            nconc (process-and-filter-call-stack args body-form)))))

(defmethod preprocess ((payload prim-call-directive))
  (preprocess-call-generic payload))

(defmethod preprocess ((payload call-directive))
  (preprocess-call-generic payload))
