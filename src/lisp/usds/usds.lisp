;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:usds)

(defvar *debug-mode* t)

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
(defvar *usds-ns-package* (find-package "USDS-NS"))

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
    (mopr:prim-set-type-name
     prim-h
     (gethash prim-type usds-cn:*prim-type-table*))))

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
           (prop-type (gethash prop-type-sym usds-cn:*prop-type-table*)))
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
            (if (mopr-val:transfer-for-type (mopr-val:value-type-real-type prop-type)
                                            datum-array-p
                                            (car values)
                                            value-h)
                (mopr:attribute-set-value attribute-h value-h)
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
      (loop for l in prim-forms
            for fn = (case (car l)
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
                   (unknown-form-error (car l) :debug))))))

(defun handle-tree-form (stage-h form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (process-prim-tree-recursive stage-h form))

(defun handle-meta-form (stage-h form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)

  ;; TODO: We don't handle metadata yet.
  (declare (ignorable stage-h form))

  nil)

(defun handle-data-form (stage-h tl-form)
  ;; (format t "~%Called handle-data-form!~%: ~S~%" tl-form)
  (loop for l in tl-form
        for fn = (case (car l)
                   (:meta   #'handle-meta-form)
                   (:|meta| #'handle-meta-form)
                   (:tree   #'handle-tree-form)
                   (:|tree| #'handle-tree-form)
                   (:prim   #'handle-prim-form)
                   (:|prim| #'handle-prim-form))
        do (if fn
               (funcall fn stage-h (cdr l))
               (unknown-form-error (car l) :debug))))

(defmacro with-usds-tables (&body body)
  `(let* ((*bind-table* (make-hash-table))
          (*alias-table* (make-hash-table))
          (usds-cn:*prim-type-table* (make-hash-table))
          (usds-cn:*prop-type-table* (make-hash-table)))
     (usds-cn:create-generic-prim-type-tokens usds-cn:*prim-type-table*)
     (mopr-val:create-generic-value-type-tokens usds-cn:*prop-type-table*)
     ,@body
     ;; (format t "HT ALIAS: ~A~%" (hash-table-count *alias-table*))
     ;; (format t "HT BIND : ~A~%" (hash-table-count *bind-table*))
     (mopr-val:delete-generic-value-type-tokens usds-cn:*prop-type-table*)
     (usds-cn:delete-generic-prim-type-tokens usds-cn:*prim-type-table*)))

(defun write-to-layer (layer-h usds-data)
  (when (mopr:layer-try-upgrade layer-h)
    (mopr:with-handle (stage-h :stage)
      (mopr:stage-open-layer stage-h layer-h)
      (with-usds-tables
          (handle-data-form stage-h usds-data)))))
