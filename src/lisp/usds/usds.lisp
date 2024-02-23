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
(defvar *data-call-table* nil)
(defvar *prim-call-table* nil)
(defvar *usds-ns-package* (find-package "USDS-NS"))

(defgeneric serialize-prop-info (ob)

  (:documentation "...")

  (:method ((ob mopr-scm:attr-info))
    (with-accessors ((name mopr-scm:prop-info-base-name)
                     (meta mopr-scm:prop-info-meta)
                     (array-p mopr-scm:attr-info-array-p)
                     (t-key mopr-scm:attr-info-type-key)) ob
      (list :attr
            (if meta (cons name meta) name)
            (if array-p :array :datum)
            t-key)))

  (:method ((ob mopr-scm:rel-info))
    (with-accessors ((name mopr-scm:prop-info-base-name)
                     (meta mopr-scm:prop-info-meta)) ob
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
                for n in (mopr-scm:prop-info-namespace info)
                do (setf x (list :ns n x))
                finally (return x))))

  (:method ((ob mopr-sgt:data-group))
    (loop for p in (mopr-sgt:data-group-data ob)
          appending (serialize p))))

(defun handle-prim-call-form (prim-h form)
  ;; (format t "~%Called handle-prim-call-form!~%: ~S~%" form)
  (if *enable-call*
      (when form
        (loop for s in (mopr-plug:process-call-stack
                        form *prim-call-table*)
              do (handle-prim-subforms
                  prim-h
                  (serialize s))))
      (unknown-form-error :call :debug)))

(defun handle-call-form (stage-h form)
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (if *enable-call*
      (when form
        (loop for s in (mopr-plug:process-call-stack
                        form *data-call-table*)
              do (handle-data-subforms
                  stage-h
                  (serialize s))))
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
    (alexandria:if-let ((s (mopr-reg:get-isa-schema prim-type)))
      (mopr:prim-set-type-name prim-h (mopr-scm:schema-name-token s))
      (unknown-form-error prim-type :debug))))

(defun handle-prim-meta-form (prim-h form)
  ;; (format t "~%Called handle-prim-meta-form!~%: ~S~%" form)

  ;; TODO: We don't handle metadata yet.
  (declare (ignorable prim-h))

  (when form
    nil))

(defun extract-prop-info
    (data ns-rlist
     &aux
       meta
       (name (etypecase data
               (symbol data)
               (string data)
               (list
                (setf meta (cdr data))
                (car data)))))
  (list :namespace ns-rlist
        :base-name name
        :meta meta))

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

(defun handle-prim-attr-form (prim-h form &optional ns-rlist)
  ;; (format t "~%Called handle-prim-attr-form!~%: ~S~%" form)
  (when form
    (destructuring-bind
        (prop-data
         attr-category
         attr-type-key
         &rest
           values
         &aux
           (info (apply #'make-instance 'mopr-scm:attr-info
                        :array-p (member attr-category '(:array :|array|))
                        :type-key attr-type-key
                        (extract-prop-info prop-data ns-rlist)))
           (attr-type (mopr-reg:get-attr-type info)))
        form

      ;; TODO: We don't handle metadata yet.
      ;; (mopr-scm:print-prop-info info)

      (if attr-type
          (mopr:with-handles* ((attribute-h :attribute)
                               (prop-name-h :token)
                               (value-h :value))
            (mopr:token-ctor-cstr prop-name-h (mopr-scm:prop-info-full-name info))
            (mopr:prim-create-attribute attribute-h
                                        prim-h
                                        prop-name-h
                                        (mopr-val:value-type-name
                                         attr-type
                                         (mopr-scm:attr-info-array-p info))
                                        0 ; bool custom
                                        mopr:+mopr-property-variability-varying+)
            (alexandria:if-let
                ((transfer-for-type-fn
                  (mopr-val:get-transfer-for-type-function
                   (mopr-val:value-type-real-type attr-type)
                   (mopr-scm:attr-info-array-p info))))
              (set-attr-for-all-timecodes transfer-for-type-fn attribute-h value-h values)
              (format t "SKIPPED UNSUPPORTED ATTRIBUTE: ~A~%"
                      (mopr-scm:prop-info-full-name info))))
          (format t "SKIPPED UNRECOGNIZED ATTRIBUTE: ~A~%"
                  (mopr-scm:prop-info-full-name info))))))

(defun handle-prim-rel-form (prim-h form &optional ns-rlist)
  ;; (format t "~%Called handle-prim-rel-form!~%: ~S~%" form)

  (declare (ignorable prim-h))

  (when form
    (destructuring-bind
        (prop-data
         &rest
           targets
         &aux
           (info (apply #'make-instance 'mopr-scm:rel-info
                        (extract-prop-info prop-data ns-rlist))))
        form

      (declare (ignorable targets))

      ;; TODO: We don't handle rel yet.
      (mopr-scm:print-prop-info info))))

(defun handle-prim-ns-form (prim-h form &optional ns-rlist)
  ;; (format t "~%Called handle-prim-ns-form!~%: ~S~%" form)
  (when form
    (destructuring-bind
        (ns &rest ns-forms)
        form
      (loop for l in ns-forms
            for fn = (case (car l)
                       (:attr   #'handle-prim-attr-form)
                       (:|attr| #'handle-prim-attr-form)
                       (:rel    #'handle-prim-rel-form)
                       (:|rel|  #'handle-prim-rel-form)
                       (:ns     #'handle-prim-ns-form)
                       (:|ns|   #'handle-prim-ns-form))
            do (if fn
                   (funcall fn prim-h (cdr l) (cons ns ns-rlist))
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
                   (:attr   #'handle-prim-attr-form)
                   (:|attr| #'handle-prim-attr-form)
                   (:rel    #'handle-prim-rel-form)
                   (:|rel|  #'handle-prim-rel-form)
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
          (mopr-reg:*registry* (mopr-reg:make-registry)))
     (mopr-plug:create-data-call-table *data-call-table*)
     (mopr-plug:create-prim-call-table *prim-call-table*)
     (mopr-reg:create-registry-tables)
     ,@body
     (mopr-reg:delete-registry-tables)))

(defun write-to-layer (layer-h usds-data)
  (unless (zerop (mopr:layer-try-upgrade layer-h))
    (mopr:with-handle (stage-h :stage)
      (mopr:stage-open-layer stage-h layer-h)
      (with-usds-variables (:enable-call nil)
        (handle-data-subforms stage-h usds-data)))))

(defun write-to-layer-call-enabled (layer-h usds-data)
  (unless (zerop (mopr:layer-try-upgrade layer-h))
    (mopr:with-handle (stage-h :stage)
      (mopr:stage-open-layer stage-h layer-h)
      (with-usds-variables (:enable-call t)
        (handle-data-subforms stage-h usds-data)))))
