;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (procedure
            (:copier nil)
            (:constructor))
  (header (make-header)
   :type header)
  (root nil
   :type (or null bnode)))

(defmacro with-bound-procedure (pr-instance-form &body body)
  `(let ((*header* (procedure-header ,pr-instance-form)))
     ,@body))

(defmacro with-bound-procedure-slots ((&rest slot-entries) pr-instance-form &body body)
  (let ((pr-sym (gensym "PROCEDURE-G")))
    `(let ((,pr-sym ,pr-instance-form))
       (with-bound-procedure ,pr-sym
         (with-slots ,slot-entries ,pr-sym
           ,@body)))))

(defmacro with-bound-procedure-accessors ((&rest slot-entries) pr-instance-form &body body)
  (let ((pr-sym (gensym "PROCEDURE-G")))
    `(let ((,pr-sym ,pr-instance-form))
       (with-bound-procedure ,pr-sym
         (with-accessors ,slot-entries ,pr-sym
           ,@body)))))

(defmacro %make-procedure-with-header-clone (pr (&rest header-args) &body body)
  `(let ((*header* (clone-header (procedure-header ,pr) ,@header-args)))
     (make-procedure :header *header* :root (progn ,@body))))

(defun make-cnode-procedure (pr)
  (%make-procedure-with-header-clone pr (:node-type 'cnode
                                         :metadata nil)
    (cnode-from-node-recursive (procedure-root pr))))

(defun make-enode-procedure (pr)
  (%make-procedure-with-header-clone pr (:node-type 'enode
                                         :metadata '((components)
                                                     (initialized)))
    (enode-from-node-recursive (procedure-root pr))))

(defun make-expanded-dnode-procedure (pr call-enabled)
  (%make-procedure-with-header-clone pr (:node-type 'dnode
                                         :metadata nil)
    (funcall (if call-enabled
                 #'node-expand-all-call-enabled
                 #'node-expand-all)
             (procedure-root pr))))

(defun make-cnode-procedure-from-usds-file (filepath &aux (pr (make-procedure)))
  (with-bound-procedure-accessors ((root procedure-root)) pr
    (setf root (read-from-usds-file filepath)))
  pr)

(defun procedure-calculate-digest (pr)
  (bnode-calculate-digest (procedure-root pr)))

(defun procedure-debug-print (pr)
  (with-bound-procedure-accessors ((root procedure-root)) pr
    (bnode-debug-print root)))

(defmacro with-limited-procedure-io-syntax ((&key read-pkg) &body body)
  `(with-standard-io-syntax
     (let ((*package* ,read-pkg)
           ;; Assignments based on uiop/stream:with-safe-io-syntax .
           (*print-readably* nil)
           (*read-eval* nil))
       ,@body)))

(defmacro with-generic-procedure-io-syntax ((&key) &body body)
  `(with-standard-io-syntax
     (let ((*print-readably* t)
           (*read-eval* nil))
       ,@body)))

(defun get-read-package ()
  (or (find-package "MOPR-USER")
      (error "Cannot find MOPR-USER package.~%")))

(defun read-from-usds-file (filepath &aux (read-pkg (get-read-package)))
  "CAUTION: Even though READ-EVAL is disabled, relying on READ for data is still dangerous!"
  (with-open-file (in filepath)
    (with-limited-procedure-io-syntax (:read-pkg read-pkg)
      (deserialize (read in nil)))))

(defun save-bnode-procedure-to-usds-file (pr filepath &key (if-exists :supersede))
  (with-open-file (out filepath :direction :output :if-exists if-exists)
    (with-bound-procedure-accessors ((root procedure-root)) pr
      (with-generic-procedure-io-syntax ()
        (pprint (node-serialize root) out)))))

(defun save-bnode-procedure-to-usds-string (pr)
  (with-bound-procedure-accessors ((root procedure-root)) pr
    (with-generic-procedure-io-syntax ()
      (prin1-to-string (node-serialize root)))))

(defun read-cnode-procedure-from-file (filepath)
  "CAUTION: Even though READ-EVAL is disabled, relying on READ for data is still dangerous!"
  (with-open-file (in filepath)
    (with-generic-procedure-io-syntax ()
      (read in nil))))

(defun dump-cnode-procedure-to-file (pr filepath &key (if-exists :supersede))
  (with-open-file (out filepath :direction :output :if-exists if-exists)
    (with-generic-procedure-io-syntax ()
      (prin1 pr out))))

(defun procedure-get-metadata-assoc (pr metadata)
  (assoc metadata (header-metadata (procedure-header pr))))

(defun procedure-update-metadata (pr metadata values get-fn set-fn)
  (let ((metadata-assoc (procedure-get-metadata-assoc pr metadata)))
    (prog1 (funcall get-fn values (cdr metadata-assoc))
      (setf (cdr metadata-assoc) (sort (funcall set-fn values (cdr metadata-assoc))
                                       #'string<= :key #'symbol-name)))))

(defun validate-enode-procedure (pr)
  (unless (eq 'enode (header-node-type (procedure-header pr)))
    (error "Procedure node type is not ENODE!")))

(defgeneric enode-procedure-create-component-unchecked (pr component-class)
  (:documentation "Create a component bound to enode procedure.")
  (:method (pr component-class)
    (with-bound-procedure-accessors ((root procedure-root)) pr
      (enode-create-components-recursive root (list component-class)))))

(defgeneric enode-procedure-delete-component-unchecked (pr component-class)
  (:documentation "Delete the component bound to enode procedure.")
  (:method (pr component-class)
    (with-bound-procedure-accessors ((root procedure-root)) pr
      (enode-delete-components-recursive root (list component-class)))))

(defgeneric enode-procedure-init-component-unchecked (pr component-class)
  (:documentation "Initialize the component bound to enode procedure.")
  (:method (pr component-class)
    (with-bound-procedure-accessors ((root procedure-root)) pr
      (enode-init-components-recursive root (list component-class)))))

(defgeneric enode-procedure-term-component-unchecked (pr component-class)
  (:documentation "Terminate the component bound to enode procedure.")
  (:method (pr component-class)
    (with-bound-procedure-accessors ((root procedure-root)) pr
      (enode-term-components-recursive root (list component-class)))))

(defun enode-procedure-create-components-internal (pr component-classes)
  (validate-enode-procedure pr)
  (loop for cc in (procedure-update-metadata pr 'components component-classes
                                             #'set-difference
                                             #'union)
        do (enode-procedure-create-component-unchecked pr cc)))

(defun enode-procedure-delete-components-internal (pr component-classes)
  (validate-enode-procedure pr)
  (loop for cc in (procedure-update-metadata pr 'components component-classes
                                             #'intersection
                                             #'set-difference)
        do (enode-procedure-delete-component-unchecked pr cc)))

(defun enode-procedure-init-components (pr component-classes)
  (validate-enode-procedure pr)
  (loop for cc in (procedure-update-metadata pr 'initialized component-classes
                                             #'set-difference
                                             #'union)
        do (enode-procedure-init-component-unchecked pr cc)))

(defun enode-procedure-term-components (pr component-classes)
  (validate-enode-procedure pr)
  (loop for cc in (procedure-update-metadata pr 'initialized component-classes
                                             #'intersection
                                             #'set-difference)
        do (enode-procedure-term-component-unchecked pr cc)))

(defun enode-procedure-create-components (pr component-classes)
  (enode-procedure-create-components-internal pr component-classes)
  (enode-procedure-init-components pr component-classes))

(defun enode-procedure-delete-components (pr component-classes)
  (enode-procedure-term-components pr component-classes)
  (enode-procedure-delete-components-internal pr component-classes))
