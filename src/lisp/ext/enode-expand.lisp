;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/enode-expand
  (:import-from :mopr)
  (:import-from :mopr-ext/enode-copy)
  (:import-from :mopr-ext/enode-serialize)
  (:use :mopr-ext/enode)
  (:use :cl)
  (:export

   #:expand-all
   #:expand-all-call-enabled

   ))

(in-package :mopr-ext/enode-expand)

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

(defgeneric expand (node parent)
  (:documentation "Expand enode."))

(defun expand-recursive (node &optional parent
                         &aux (expanded (expand node parent)))
  "Create a macro-expanded enode hierarchy."
  (loop for e in expanded
        do (loop for c across (enode-children node)
                 do (expand-recursive c e)))
  expanded)

;; Assumes being called within a with-registry scope.
(defun expand-all (rn)
  (mopr-plug:with-configuration ()
    (with-expansion-variables (:enable-call nil)
      (car (expand-recursive rn)))))

(defun expand-all-call-enabled (rn)
  (mopr-plug:with-configuration ()
    (with-expansion-variables (:enable-call t)
      (car (expand-recursive rn)))))

;;
;;; EXPAND IMPLEMENTATIONS
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

(defmethod expand ((node enode) parent
                   &aux (e (mopr-ext/enode-copy:copy-enode-instance node parent)))
  (when parent (vector-push-extend e (enode-children parent)))
  (list e))

(defmethod expand ((node var-enode) parent)
  (validate-call-support node :debug)
  (with-accessors ((name var-enode-name-param)
                   (aux var-enode-aux-form-param)
                   (val var-enode-val-form-param)) node
    (setf (gethash name *var-table*)
          (car (mopr-plug:process-call-stack aux val *var-table*))))
  nil)

(defmethod expand ((node each-enode) parent)
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

(defmethod expand ((node iota-enode) parent)
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

(defgeneric serialize-prop-info (ob)

  (:documentation "...")

  (:method ((ob mopr-info:attr-info))
    (with-accessors ((name mopr-info:prop-info-base-name)
                     (meta mopr-info:prop-info-meta)
                     (array-p mopr-info:attr-info-array-p)
                     (t-key mopr-info:attr-info-type-key)) ob
      (list :attr
            (if meta (cons name meta) name)
            (if array-p :array :datum)
            t-key)))

  (:method ((ob mopr-info:rel-info))
    (with-accessors ((name mopr-info:prop-info-base-name)
                     (meta mopr-info:prop-info-meta)) ob
      (list :rel (if meta (cons name meta) name)))))

(defgeneric serialize (ob)

  (:documentation "...")

  (:method ((ob t))
    nil)

  (:method ((ob mopr-sgt:tree-entry))
    ;; (format t "~%~S~%" ob)
    (list (cons :tree (mopr-sgt:tree-entry-data ob))))

  (:method ((ob mopr-sgt:prim-entry))
    ;; (format t "~%~S~%" ob)
    (list (cons :prim (mopr-sgt:prim-entry-data ob))))

  (:method ((ob mopr-sgt:prop-entry)
            &aux
              (info (mopr-sgt:prop-entry-info ob)))
    ;; (format t "~%~S~%" ob)
    (list (loop with x = (append (serialize-prop-info info)
                                 (mopr-sgt:prop-entry-data ob))
                for n in (mopr-info:prop-info-namespace info)
                do (setf x (list :ns n x))
                finally (return x))))

  (:method ((ob mopr-sgt:data-group))
    (loop for p in (mopr-sgt:data-group-data ob)
          appending (serialize p))))

;; TODO: Reapply expansion to results.
(defun expand-call-generic (node parent fn
                            &aux expanded-forms)
  (validate-call-support node :debug)
  (with-accessors ((aux-form call-enode-aux-form-param)
                   (body-form call-enode-body-form-param)) node
    (let ((args-list (if (listp aux-form)
                         (list aux-form)
                         (gethash aux-form *each-table*))))
      ;; TODO: Remove the need for serialization before expansion,
      (setf expanded-forms
            (loop for args in args-list
                  nconc (loop for s in (mopr-plug:process-call-stack
                                        args body-form *var-table*)
                              nconc (serialize s))))
      (funcall fn parent expanded-forms nil)
      (list (enode-children parent)))))

(defmethod expand ((node prim-call-enode) parent)
  (expand-call-generic node parent #'mopr-ext/enode-serialize::handle-prim-subforms))

(defmethod expand ((node call-enode) parent)
  (expand-call-generic node parent #'mopr-ext/enode-serialize::handle-data-subforms))