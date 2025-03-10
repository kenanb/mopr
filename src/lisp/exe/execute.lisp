;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-exe)

;;
;;; MAIN API
;;

(defvar *bind-table* nil)
(defvar *alias-table* nil)

(defgeneric execute (payload node target-h containers)
  (:documentation "Execute node payload."))

(defun node-execute (node target-h &optional containers
                     &aux (p (bnode-find-payload node)))
  (etypecase p
    (container (node-continue-execution node target-h (cons p containers)))
    ;; NOTE : If recursive re-expansion works correctly, no "directive"
    ;;        should be left in the tree by the time we execute it.
    (directive (error "Encountered directive that should have been expanded."))
    (statement (execute p node target-h containers))))

(defun node-continue-execution (node target-h containers)
  (loop for ch across (bnode-children node) do (node-execute ch target-h containers)))

(defmacro with-execution-variables ((&key)
                                    &body body)
  `(let* ((*bind-table* (make-hash-table))
          (*alias-table* (make-hash-table)))
     ,@body))

(defun execute-all (node stage-h)
  (mopr-usd/info:with-registry (:supported-cases '(:upcase))
    (with-execution-variables ()
      (node-execute node stage-h))))

;;
;;; EXECUTE IMPLEMENTATIONS
;;

(defvar *debug-mode* t)

(defun payload-content-error (payload action)
  (format t "
[ERROR] Error in payload content.
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
    (error "Cannot handle payload: ~S~%" payload)))

(defun prim-path-string (r-prim-path &key reverse-p)
  (format nil "~{/~A~}" (if reverse-p
                            (reverse r-prim-path)
                            r-prim-path)))

(defmethod execute ((payload prim-type-statement) node prim-h containers
                    &aux (schema-name (prim-type-statement-name-param payload)))
  (declare (ignore node))
  (when schema-name
    (alexandria:if-let ((s (mopr-usd/info:get-schema :isa schema-name)))
      (mopr-usd:prim-set-type-name prim-h (mopr-usd/info:schema-name-token s))
      (payload-content-error payload :debug))))

(defun set-attr-for-all-timecodes (fn attribute-h value-h value-list)
  (mopr-usd:with-handles* ((timecode-h :timecode))
    (loop for val-elt in value-list
          do (let (val)
               (etypecase val-elt
                 (array
                  (mopr-usd:timecode-ctor-default timecode-h)
                  (setf val val-elt))
                 (cons
                  (mopr-usd:timecode-ctor-double timecode-h
                                                 (coerce (car val-elt) 'double-float))
                  (setf val (cdr val-elt))))
               (funcall fn val value-h)
               (mopr-usd:attribute-set-value attribute-h value-h timecode-h)))))

(defun execute-attr (info body-form prim-h
                     &aux
                       (attr-type (mopr-usd/info:get-value-type-for-attr-info info)))

  ;; TODO: We don't handle metadata yet.
  ;; (mopr-usd/info:print-prop-info info)

  (if attr-type
      (mopr-usd:with-handles* ((attribute-h :attribute)
                               (prop-name-h :token)
                               (value-h :value))
        (mopr-usd:token-ctor-cstr prop-name-h (mopr-usd/info:prop-info-full-name info))
        (mopr-usd:prim-create-attribute attribute-h
                                        prim-h
                                        prop-name-h
                                        (mopr-usd/info:value-type-name
                                         attr-type
                                         (mopr-usd/info:attr-info-array-p info))
                                        0 ; bool custom
                                        mopr-usd:+mopr-property-variability-varying+)
        (alexandria:if-let
            ((transfer-for-type-fn
              (mopr-usd/val:get-transfer-for-type-function
               (mopr-usd/info:value-type-real-type attr-type)
               (mopr-usd/info:attr-info-array-p info))))
          (set-attr-for-all-timecodes transfer-for-type-fn attribute-h value-h body-form)
          (format t "SKIPPED UNSUPPORTED ATTRIBUTE: ~A~%"
                  (mopr-usd/info:prop-info-full-name info))))
      (format t "SKIPPED UNRECOGNIZED ATTRIBUTE: ~A~%"
              (mopr-usd/info:prop-info-full-name info))))

;; TODO: This node ignores the prim-ns-statement ancestors as it gets namespace information
;;       from schema. Its construction within a prim-ns-statement should probably be disallowed.
(defmethod execute ((payload prim-schema-prop-statement) node prim-h containers
                    &aux ; TODO: Add serialization tests.
                      (info-args (prim-schema-prop-statement-info-args-param payload))
                      (info (apply #'mopr-usd/info:get-prop-info-for-schema info-args)))
  (declare (ignore node))
  (etypecase info
    (mopr-usd/info:attr-info
     (execute-attr info (prim-schema-prop-statement-body-form-param payload) prim-h))
    (mopr-usd/info:rel-info
     ;; TODO: We don't handle relationships yet.
     nil)))

(defun collect-namespace (containers &optional ns-list &aux (pp (car containers)))
  (etypecase pp
    (prim-ns-container (collect-namespace (cdr containers)
                                          (cons (prim-ns-container-name-param pp) ns-list)))
    (t ns-list)))

(defmethod execute ((payload prim-attr-statement) node prim-h containers)
  (let* ((ns-list (collect-namespace containers))
         (array-attr-p (not (null (member (prim-attr-statement-category-param payload)
                                          '(:array :|array|)))))
         (info (make-instance
                'mopr-usd/info:attr-info
                :array-p array-attr-p
                :type-key (prim-attr-statement-type-param payload)
                :namespace (reverse ns-list) ; TODO: Revise ATTR-INFO.
                :base-name (prim-attr-statement-name-param payload)
                :meta (prim-attr-statement-meta-form-param payload))))
    (execute-attr info (prim-attr-statement-body-form-param payload) prim-h)))

(defmethod execute ((payload prim-rel-statement) node prim-h containers)
  (declare (ignore node prim-h))
  ;; TODO: We don't handle relationships yet.
  nil)

(defmethod execute ((payload prim-meta-statement) node prim-h containers)
  (declare (ignore node prim-h))
  ;; TODO: We don't handle metadata yet.
  nil)

(defmethod execute ((payload prim-statement) node stage-h containers)
  (with-accessors ((prim-form prim-statement-path-form-param)) payload
    (let* ((prim-path (etypecase prim-form
                        (symbol (gethash prim-form *alias-table*))
                        (list prim-form)))
           (prim-path-str (prim-path-string prim-path)))
      (mopr-usd:with-handles* ((path-h :path)
                               (prim-h :prim))
        (mopr-usd:path-ctor-cstr path-h prim-path-str)
        (mopr-usd:stage-get-prim-at-path prim-h stage-h path-h)
        (node-continue-execution node prim-h containers)))))

(defclass tnode ()
  ((path
    :initarg :path
    :type list
    :initform nil
    :reader tnode-path)
   (spec
    :initarg :spec
    :initarg :|spec|
    :type keyword
    :initform :def
    :reader tnode-spec)
   (bind
    :initarg :bind
    :initarg :|bind|
    :type symbol
    :initform nil
    :reader tnode-bind)
   (alias
    :initarg :alias
    :initarg :|alias|
    :type symbol
    :initform nil
    :reader tnode-alias)))

(defun get-tnode-and-subtree (r-prim-path args)
  (loop for x on args by #'cddr
        for k = (car x)
        for v = (cadr x)
        while (keywordp k)
        nconc (list k v) into initargs
        finally (return (cons (apply #'make-instance
                                     'tnode
                                     :path r-prim-path
                                     initargs)
                              x))))

(defun process-spec (stage-h tn)
  (when (tnode-alias tn)
    (setf (gethash (tnode-alias tn) *alias-table*) (reverse (tnode-path tn))))
  (when (tnode-bind tn)
    (setf (gethash (tnode-bind tn) *bind-table*) (reverse (tnode-path tn))))
  ;; (describe tn)
  (alexandria:if-let
      ((fn (case (tnode-spec tn)
             (:def     #'mopr-usd:stage-define-prim)
             (:|def|   #'mopr-usd:stage-define-prim)
             (:over    #'mopr-usd:stage-override-prim)
             (:|over|  #'mopr-usd:stage-override-prim)
             (:class   #'mopr-usd:stage-create-class-prim)
             (:|class| #'mopr-usd:stage-create-class-prim))))
    (mopr-usd:with-handles* ((path-h :path)
                             (prim-h :prim))
      (mopr-usd:path-ctor-cstr path-h (prim-path-string (tnode-path tn) :reverse-p t))
      (funcall fn prim-h stage-h path-h))
    (payload-content-error (tnode-spec tn) :debug)))

(defun process-prim-tree-recursive (stage-h tree &optional r-ancestors)
  (when (car tree)
    (let* ((r-prim-path (cons (caar tree) r-ancestors))
           (args (cdar tree))
           (tn-and-subtree (get-tnode-and-subtree r-prim-path args))
           (tn (car tn-and-subtree))
           (subtree (cdr tn-and-subtree)))
      (process-spec stage-h tn)
      (process-prim-tree-recursive stage-h
                                   subtree
                                   r-prim-path))
    (process-prim-tree-recursive stage-h (cdr tree) r-ancestors)))

(defmethod execute ((payload tree-statement) node stage-h containers)
  (declare (ignore node))
  (process-prim-tree-recursive stage-h (tree-statement-body-form-param payload)))

(defmethod execute ((payload meta-statement) node stage-h containers)
  (declare (ignore node))
  ;; TODO: We don't handle metadata yet.
  nil)
