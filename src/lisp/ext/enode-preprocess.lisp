;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/enode-preprocess
  (:import-from :mopr)
  (:import-from :mopr-ext/enode-copy)
  (:use :mopr-ext/enode)
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

(defgeneric convert-sgt (ob)

  (:method ((ob t))
    nil)

  (:method ((ob mopr-sgt:tree-entry))
    (make-instance 'tree-enode :body-form-param (mopr-sgt:tree-entry-data ob)))

  (:method ((ob mopr-sgt:prop-entry)
            &aux
              (info (mopr-sgt:prop-entry-info ob))
              (args (list :name-param (mopr-info:prop-info-base-name info)
                          :meta-form-param (mopr-info:prop-info-meta info)
                          :body-form-param (mopr-sgt:prop-entry-data ob)))
              (prop (etypecase info
                      (mopr-info:attr-info
                       (apply #'make-instance 'prim-attr-enode
                              :category-param (if (mopr-info:attr-info-array-p info)
                                                  :array
                                                  :datum)
                              :type-param (mopr-info:attr-info-type-key info)
                              args))
                      (mopr-info:rel-info
                       (apply #'make-instance 'prim-rel-enode args)))))
    (loop with x = prop
          for ns in (mopr-info:prop-info-namespace info)
          do (let ((ns-node (make-instance 'prim-ns-enode :name-param ns)))
               (vector-push-extend x (enode-children ns-node))
               (setf x ns-node))
          finally (return x)))

  (:method ((ob mopr-sgt:data-group)
            &aux (group-node (make-instance 'group-enode)))
    (loop for p in (mopr-sgt:data-group-data ob)
          do (vector-push-extend (convert-sgt p) (enode-children group-node)))
    group-node))

;; TODO: Reapply expansion to results.
(defun preprocess-call-generic (node)
  (validate-call-support node :debug)
  (with-accessors ((aux-form call-enode-aux-form-param)
                   (body-form call-enode-body-form-param)) node
    (let ((args-list (if (listp aux-form)
                         (list aux-form)
                         (gethash aux-form *each-table*))))
      (loop for args in args-list
            nconc (loop for s in (mopr-plug:process-call-stack args body-form *var-table*)
                        for x = (convert-sgt s)
                        when x collect x)))))

(defmethod preprocess ((node prim-call-enode))
  (preprocess-call-generic node))

(defmethod preprocess ((node call-enode))
  (preprocess-call-generic node))