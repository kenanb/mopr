;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

;;
;;; MAIN API
;;

(defvar *bind-table* nil)
(defvar *alias-table* nil)

(defgeneric execute (payload node target-h containers)
  (:documentation "Execute cnode payload."))

(defun cnode-execute (node target-h &optional containers
                      &aux (p (bnode-find-payload node)))
  (etypecase p
    (container (cnode-continue-execution node target-h (cons p containers)))
    ;; NOTE : If recursive re-expansion works correctly, no "directive"
    ;;        should be left in the tree by the time we execute it.
    (directive (error "Encountered directive that should have been preprocessed."))
    (statement (execute p node target-h containers))))

(defun cnode-continue-execution (node target-h containers)
  (loop for ch across (bnode-children node) do (cnode-execute ch target-h containers)))

(defmacro with-execution-variables ((&key)
                                    &body body)
  `(let* ((*bind-table* (make-hash-table))
          (*alias-table* (make-hash-table)))
     ,@body))

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
    (alexandria:if-let ((s (mopr-info:get-schema :isa schema-name)))
      (mopr:prim-set-type-name prim-h (mopr-info:schema-name-token s))
      (payload-content-error payload :debug))))

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

(defun execute-attr (info body-form prim-h
                     &aux
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
              (mopr-info:prop-info-full-name info))))

;; TODO: This node ignores the prim-ns-statement ancestors as it gets namespace information
;;       from schema. Its construction within a prim-ns-statement should probably be disallowed.
(defmethod execute ((payload prim-schema-prop-statement) node prim-h containers
                    &aux (info (prim-schema-prop-statement-info-param payload)))
  (declare (ignore node))
  (etypecase info
    (mopr-info:attr-info
     (execute-attr info (prim-schema-prop-statement-body-form-param payload) prim-h))
    (mopr-info:rel-info
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
                'mopr-info:attr-info
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
  (declare (ignore node))
  (with-accessors ((prim-form prim-statement-path-form-param)) payload
    (let* ((prim-path (etypecase prim-form
                        (symbol (gethash prim-form *alias-table*))
                        (list prim-form)))
           (prim-path-str (prim-path-string prim-path)))
      (mopr:with-handles* ((path-h :path)
                           (prim-h :prim))
        (mopr:path-ctor-cstr path-h prim-path-str)
        (mopr:stage-get-prim-at-path prim-h stage-h path-h)
        (cnode-continue-execution node prim-h containers)))))

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
             (:def     #'mopr:stage-define-prim)
             (:|def|   #'mopr:stage-define-prim)
             (:over    #'mopr:stage-override-prim)
             (:|over|  #'mopr:stage-override-prim)
             (:class   #'mopr:stage-create-class-prim)
             (:|class| #'mopr:stage-create-class-prim))))
    (mopr:with-handles* ((path-h :path)
                         (prim-h :prim))
      (mopr:path-ctor-cstr path-h (prim-path-string (tnode-path tn) :reverse-p t))
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
