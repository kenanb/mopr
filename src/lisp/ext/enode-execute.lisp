;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/enode-execute
  (:import-from :mopr)
  (:import-from :mopr-ext/enode-expand)
  (:use :mopr-ext/enode)
  (:use :cl)
  (:export

   #:populate-layer

   ))

(in-package :mopr-ext/enode-execute)

;;
;;; MAIN API
;;

(defvar *bind-table* nil)
(defvar *alias-table* nil)

(defgeneric execute (node target-h)
  (:documentation "Execute enode."))

(defmacro with-execution-variables ((&key)
                                    &body body)
  `(let* ((*bind-table* (make-hash-table))
          (*alias-table* (make-hash-table)))
     ,@body))

(defun populate-layer (layer-h rn call-enabled)
  (unless (zerop (mopr:layer-try-upgrade layer-h))
    (mopr:with-handle (stage-h :stage)
      (mopr:stage-open-layer stage-h layer-h)
      (mopr-info:with-registry (:supported-cases '(:upcase))
        ;; (debug-print rn)
        (let* ((expand-all-fn (if call-enabled
                                  #'mopr-ext/enode-expand:expand-all-call-enabled
                                  #'mopr-ext/enode-expand:expand-all))
               (rn-expanded (funcall expand-all-fn rn)))
          ;; (debug-print rn-expanded)
          (with-execution-variables ()
            (execute rn-expanded stage-h)))))))

;;
;;; EXECUTE IMPLEMENTATIONS
;;

(defvar *debug-mode* t)

(defun node-content-error (node action)
  (format t "
[ERROR] Error in node content.
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
    (error "Cannot handle node: ~S~%" node)))

(defun prim-path-string (r-prim-path &key reverse-p)
  (format nil "~{/~A~}" (if reverse-p
                            (reverse r-prim-path)
                            r-prim-path)))

(defmethod execute ((node enode) target-h)
  (loop for c across (enode-children node) do (execute c target-h)))

(defmethod execute ((node prim-type-enode) prim-h
                    &aux (schema-name (prim-type-enode-name-param node)))
  (when schema-name
    (alexandria:if-let ((s (mopr-info:get-schema :isa schema-name)))
      (mopr:prim-set-type-name prim-h (mopr-info:schema-name-token s))
      (node-content-error node :debug))))

(defun set-attr-for-all-timecodes (fn attribute-h value-h value-list)
  (mopr:with-handles* ((timecode-h :timecode))
    (loop for val-elt in value-list
          do (let (val)
               (etypecase val-elt
                 (array
                  (mopr:timecode-ctor-default timecode-h)
                  (setf val val-elt))
                 (cons
                  (mopr:timecode-ctor-double timecode-h
                                             (coerce (car val-elt) 'double-float))
                  (setf val (cdr val-elt))))
               (funcall fn val value-h)
               (mopr:attribute-set-value attribute-h value-h timecode-h)))))

(defun collect-namespace (node &optional ns-list
                          &aux (pn (enode-parent node)))
  (etypecase pn
    (prim-ns-enode (collect-namespace pn (cons (prim-ns-enode-name-param pn) ns-list)))
    (t ns-list)))

(defmethod execute ((node prim-attr-enode) prim-h
                    &aux (ns-list (collect-namespace node)))
  (with-accessors ((name prim-attr-enode-name-param)
                   (meta-form prim-attr-enode-meta-form-param)
                   (category prim-attr-enode-category-param)
                   (attr-type-key prim-attr-enode-type-param)
                   (body-form prim-attr-enode-body-form-param)) node
    (let* ((info (make-instance 'mopr-info:attr-info
                                :array-p (not (null (member category '(:array :|array|))))
                                :type-key attr-type-key
                                :namespace (reverse ns-list) ; TODO: Revise ATTR-INFO.
                                :base-name name
                                :meta meta-form))
           (attr-type (mopr-info:get-value-type-for-attr-info info)))

      ;; TODO: We don't handle metadata yet.
      ;; (mopr-info:print-prop-info info)


      (if attr-type
          (mopr:with-handles* ((attribute-h :attribute)
                               (prop-name-h :token)
                               (value-h :value))
            (mopr:token-ctor-cstr prop-name-h (mopr-info:prop-info-full-name info))
            (mopr:prim-create-attribute attribute-h
                                        prim-h
                                        prop-name-h
                                        (mopr-info:value-type-name
                                         attr-type
                                         (mopr-info:attr-info-array-p info))
                                        0 ; bool custom
                                        mopr:+mopr-property-variability-varying+)
            (alexandria:if-let
                ((transfer-for-type-fn
                  (mopr-val:get-transfer-for-type-function
                   (mopr-info:value-type-real-type attr-type)
                   (mopr-info:attr-info-array-p info))))
              (set-attr-for-all-timecodes transfer-for-type-fn attribute-h value-h body-form)
              (format t "SKIPPED UNSUPPORTED ATTRIBUTE: ~A~%"
                      (mopr-info:prop-info-full-name info))))
          (format t "SKIPPED UNRECOGNIZED ATTRIBUTE: ~A~%"
                  (mopr-info:prop-info-full-name info))))))

(defmethod execute ((node prim-rel-enode) prim-h)
  ;; TODO: We don't handle relationships yet.
  (call-next-method))

(defmethod execute ((node prim-meta-enode) prim-h)
  ;; TODO: We don't handle metadata yet.
  (call-next-method))

(defmethod execute ((node prim-enode) stage-h)
  (with-accessors ((prim-form prim-enode-path-form-param)) node
    (let* ((prim-path (etypecase prim-form
                        (symbol (gethash prim-form *alias-table*))
                        (list prim-form)))
           (prim-path-str (prim-path-string prim-path)))
      (mopr:with-handles* ((path-h :path)
                           (prim-h :prim))
        (mopr:path-ctor-cstr path-h prim-path-str)
        (mopr:stage-get-prim-at-path prim-h stage-h path-h)
        (call-next-method node prim-h)))))

(defclass node ()
  ((path
    :initarg :path
    :type list
    :initform nil
    :reader node-path)
   (spec
    :initarg :spec
    :initarg :|spec|
    :type keyword
    :initform :def
    :reader node-spec)
   (bind
    :initarg :bind
    :initarg :|bind|
    :type symbol
    :initform nil
    :reader node-bind)
   (alias
    :initarg :alias
    :initarg :|alias|
    :type symbol
    :initform nil
    :reader node-alias)))

(defun get-node-and-subtree (r-prim-path args)
  (loop for x on args by #'cddr
        for k = (car x)
        for v = (cadr x)
        while (keywordp k)
        nconc (list k v) into initargs
        finally (return (cons (apply #'make-instance
                                     'node
                                     :path r-prim-path
                                     initargs)
                              x))))

(defun process-spec (stage-h node)
  (when (node-alias node)
    (setf (gethash (node-alias node) *alias-table*) (reverse (node-path node))))
  (when (node-bind node)
    (setf (gethash (node-bind node) *bind-table*) (reverse (node-path node))))
  ;; (describe node)
  (alexandria:if-let
      ((fn (case (node-spec node)
             (:def     #'mopr:stage-define-prim)
             (:|def|   #'mopr:stage-define-prim)
             (:over    #'mopr:stage-override-prim)
             (:|over|  #'mopr:stage-override-prim)
             (:class   #'mopr:stage-create-class-prim)
             (:|class| #'mopr:stage-create-class-prim))))
    (mopr:with-handles* ((path-h :path)
                         (prim-h :prim))
      (mopr:path-ctor-cstr path-h (prim-path-string (node-path node) :reverse-p t))
      (funcall fn prim-h stage-h path-h))
    (node-content-error (node-spec node) :debug)))

(defun process-prim-tree-recursive (stage-h tree &optional r-ancestors)
  (when (car tree)
    (let* ((r-prim-path (cons (caar tree) r-ancestors))
           (args (cdar tree))
           (node-and-subtree (get-node-and-subtree r-prim-path args))
           (node (car node-and-subtree))
           (subtree (cdr node-and-subtree)))
      (process-spec stage-h node)
      (process-prim-tree-recursive stage-h
                                   subtree
                                   r-prim-path))
    (process-prim-tree-recursive stage-h (cdr tree) r-ancestors)))

(defmethod execute ((node tree-enode) stage-h)
  (process-prim-tree-recursive stage-h (tree-enode-body-form-param node)))

(defmethod execute ((node meta-enode) stage-h)
  ;; TODO: We don't handle metadata yet.
  (call-next-method))

;; TODO : We currently don't reapply recursive expansion to expansion results.
;;        The test "09_call", which tests this case, is currently disabled.
(defmethod execute ((node var-enode) target-h)
  (declare (ignore node target-h))
  (error "Encountered node that should have been expanded."))

(defmethod execute ((node each-enode) target-h)
  (declare (ignore node target-h))
  (error "Encountered node that should have been expanded."))

(defmethod execute ((node iota-enode) target-h)
  (declare (ignore node target-h))
  (error "Encountered node that should have been expanded."))

(defmethod execute ((node prim-call-enode) target-h)
  (declare (ignore node target-h))
  (error "Encountered node that should have been expanded."))

(defmethod execute ((node call-enode) target-h)
  (declare (ignore node target-h))
  (error "Encountered node that should have been expanded."))
