;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:usds)

(defvar *debug-mode* t)

(defvar *enable-call* nil)

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

(defvar *bind-table* nil)
(defvar *alias-table* nil)
(defvar *prim-type-table* nil)
(defvar *prop-type-table* nil)
(defvar *data-call-table* nil)
(defvar *prim-call-table* nil)
(defvar *usds-ns-package* (find-package "USDS-NS"))

(defun handle-prim-call-form (prim-h form)
  ;; (format t "~%Called handle-prim-call-form!~%: ~S~%" form)
  (if *enable-call*
      (when form
        (handle-prim-subforms
         prim-h
         (alexandria:when-let ((fn (gethash (car form) *prim-call-table*)))
           (apply fn (cdr form)))))
      (unknown-form-error :call :debug)))

(defun handle-call-form (stage-h form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (if *enable-call*
      (when form
        (handle-data-subforms
         stage-h
         (alexandria:when-let ((fn (gethash (car form) *data-call-table*)))
           (apply fn (cdr form)))))
      (unknown-form-error :call :debug)))

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

(defun prim-path-string (r-prim-path &key reverse-p)
  (format nil "~{/~A~}" (if reverse-p
                            (reverse r-prim-path)
                            r-prim-path)))

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
    (unknown-form-error (node-spec node) :debug)))

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

(defun handle-prim-type-form (prim-h form &aux (prim-type (car form)))
  ;; (format t "~%Called handle-prim-type-form!~%: ~S~%" form)
  (when prim-type
    (alexandria:if-let ((type-entry (gethash prim-type *prim-type-table*)))
      (mopr:prim-set-type-name prim-h type-entry)
      (unknown-form-error prim-type :debug))))

(defun handle-prim-meta-form (prim-h form)
  ;; (format t "~%Called handle-prim-meta-form!~%: ~S~%" form)

  ;; TODO: We don't handle metadata yet.
  (declare (ignorable prim-h))

  (when form
    nil))

(defun prop-name-string (r-prop-name &key reverse-p)
  (format nil "~{~A~^:~}" (if reverse-p
                              (reverse r-prop-name)
                              r-prop-name)))

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

(defun handle-prim-prop-form (prim-h form &optional ns-list)
  ;; (format t "~%Called handle-prim-prop-form!~%: ~S~%" form)
  (when form
    (destructuring-bind
        (prop-id
         &rest
           data
         &aux
           prop-meta
           (prop-name (etypecase prop-id
                        (symbol prop-id)
                        (string prop-id)
                        (list
                         (setf prop-meta (cdr prop-id))
                         (car prop-id))))
           (prop-name-str (prop-name-string
                           (cons prop-name ns-list)
                           :reverse-p t))
           (datum-array-p (member (car data) '(:array :|array|)))
           (data (if datum-array-p (cdr data) data))
           (values (cdr data))
           (prop-type-sym (car data))
           (prop-type (gethash prop-type-sym *prop-type-table*)))
        form

      ;; TODO: We don't handle metadata yet.
      (declare (ignorable prop-meta))

      ;; (format t "~%[PROP-NAME-STR] ~S~%" prop-name-str)
      ;; (format t "~%[PROP-META] ~S~%" prop-meta)

      (if prop-type
          (mopr:with-handles* ((attribute-h :attribute)
                               (prop-name-h :token)
                               (value-h :value))
            (mopr:token-ctor-cstr prop-name-h prop-name-str)
            (mopr:prim-create-attribute attribute-h
                                        prim-h
                                        prop-name-h
                                        (mopr-val:value-type-name prop-type datum-array-p)
                                        0 ; bool custom
                                        mopr:+mopr-attribute-variability-varying+)
            (alexandria:if-let
                ((transfer-for-type-fn
                  (mopr-val:get-transfer-for-type-function
                   (mopr-val:value-type-real-type prop-type)
                   datum-array-p)))
              (set-attr-for-all-timecodes transfer-for-type-fn attribute-h value-h values)
              (format t "SKIPPED UNSUPPORTED ATTRIBUTE: ~A~%" prop-name-str)))
          (format t "SKIPPED UNRECOGNIZED ATTRIBUTE: ~A~%" prop-name-str)))))

(defun handle-prim-ns-form (prim-h form &optional ns-list)
  ;; (format t "~%Called handle-prim-ns-form!~%: ~S~%" form)
  (when form
    (destructuring-bind
        (ns &rest ns-forms)
        form
      (loop for l in ns-forms
            for fn = (case (car l)
                       (:prop   #'handle-prim-prop-form)
                       (:|prop| #'handle-prim-prop-form)
                       (:ns     #'handle-prim-ns-form)
                       (:|ns|   #'handle-prim-ns-form))
            do (if fn
                   (funcall fn prim-h (cdr l) (cons ns ns-list))
                   (unknown-form-error (car l) :debug))))))

(defun handle-prim-subforms (prim-h subforms)
  ;; (format t "~%Called handle-prim-subforms!~%: ~S~%" form)
  (loop for l in subforms
        for fn = (case (car l)
                   (:call   #'handle-prim-call-form)
                   (:|call| #'handle-prim-call-form)
                   (:type   #'handle-prim-type-form)
                   (:|type| #'handle-prim-type-form)
                   (:meta   #'handle-prim-meta-form)
                   (:|meta| #'handle-prim-meta-form)
                   (:prop   #'handle-prim-prop-form)
                   (:|prop| #'handle-prim-prop-form)
                   (:ns     #'handle-prim-ns-form)
                   (:|ns|   #'handle-prim-ns-form))
        do (if fn
               (funcall fn prim-h (cdr l))
               (unknown-form-error (car l) :debug))))

(defun handle-prim-form (stage-h form)
  ;; (format t "~%Called handle-prim-form!~%: ~S~%" form)
  (destructuring-bind
      (prim-id
       &rest
         prim-forms
       &aux
         (prim-path (etypecase prim-id
                      (symbol (gethash prim-id *alias-table*))
                      (list prim-id)))
         (prim-path-str (prim-path-string prim-path)))
      form
    (mopr:with-handles* ((path-h :path)
                         (prim-h :prim))
      (mopr:path-ctor-cstr path-h prim-path-str)
      (mopr:stage-get-prim-at-path prim-h stage-h path-h)
      (handle-prim-subforms prim-h prim-forms))))

(defun handle-tree-form (stage-h form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (process-prim-tree-recursive stage-h form))

(defun handle-meta-form (stage-h form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)

  ;; TODO: We don't handle metadata yet.
  (declare (ignorable stage-h form))

  nil)

(defun handle-data-subforms (stage-h subforms)
  ;; (format t "~%Called handle-data-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for fn = (case (car l)
                   (:call   #'handle-call-form)
                   (:|call| #'handle-call-form)
                   (:meta   #'handle-meta-form)
                   (:|meta| #'handle-meta-form)
                   (:tree   #'handle-tree-form)
                   (:|tree| #'handle-tree-form)
                   (:prim   #'handle-prim-form)
                   (:|prim| #'handle-prim-form))
        do (if fn
               (funcall fn stage-h (cdr l))
               (unknown-form-error (car l) :debug))))

(defmacro with-usds-variables ((&key
                                  (enable-call nil))
                               &body body)
  `(let* ((*enable-call* ,enable-call)
          (*bind-table* (make-hash-table))
          (*alias-table* (make-hash-table))
          (*data-call-table* (make-hash-table))
          (*prim-call-table* (make-hash-table))
          (*prim-type-table* (make-hash-table))
          (*prop-type-table* (make-hash-table)))
     (mopr-plug:create-data-call-table *data-call-table*)
     (mopr-plug:create-prim-call-table *prim-call-table*)
     (mopr-prim:create-generic-prim-type-tokens *prim-type-table*)
     (mopr-val:create-generic-value-type-tokens *prop-type-table*)
     ,@body
     ;; (format t "HT ALIAS: ~A~%" (hash-table-count *alias-table*))
     ;; (format t "HT BIND : ~A~%" (hash-table-count *bind-table*))
     (mopr-val:delete-generic-value-type-tokens *prop-type-table*)
     (mopr-prim:delete-generic-prim-type-tokens *prim-type-table*)))

(defun write-to-layer (layer-h usds-data)
  (when (mopr:layer-try-upgrade layer-h)
    (mopr:with-handle (stage-h :stage)
      (mopr:stage-open-layer stage-h layer-h)
      (with-usds-variables (:enable-call nil)
        (handle-data-subforms stage-h usds-data)))))

(defun write-to-layer-call-enabled (layer-h usds-data)
  (when (mopr:layer-try-upgrade layer-h)
    (mopr:with-handle (stage-h :stage)
      (mopr:stage-open-layer stage-h layer-h)
      (with-usds-variables (:enable-call t)
        (handle-data-subforms stage-h usds-data)))))
