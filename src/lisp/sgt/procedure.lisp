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

(defun procedure-call (pr fn &rest args)
  (let ((*header* (procedure-header pr)))
    (apply fn (procedure-root pr) args)))

(defun procedure-call-constructor (pr fn &rest args)
  (let ((*header* (clone-header (procedure-header pr))))
    (make-procedure :header *header* :root (apply fn (procedure-root pr) args))))

(defun make-cnode-procedure (pr)
  (procedure-call-constructor pr #'cnode-from-node-recursive))

(defun make-enode-procedure (pr)
  (procedure-call-constructor pr #'enode-from-node-recursive))

(defun make-preprocessed-dnode-procedure (pr call-enabled)
  (procedure-call-constructor pr (if call-enabled
                                     #'preprocess-all-call-enabled
                                     #'preprocess-all)))

(defun make-cnode-procedure-from-usds-file (filepath read-pkg &aux (pr (make-procedure)))
  (let ((*header* (procedure-header pr)))
    (setf (procedure-root pr) (read-from-usds-file filepath read-pkg)))
  pr)

(defun procedure-debug-print (pr)
  (procedure-call pr #'bnode-debug-print))

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

(defun read-from-usds-file (filepath read-pkg)
  "CAUTION: Even though READ-EVAL is disabled, relying on READ for data is still dangerous!"
  (with-open-file (in filepath)
    (with-limited-procedure-io-syntax (:read-pkg read-pkg)
      (deserialize (read in nil)))))

(defun save-bnode-procedure-to-usds-file (pr filepath &key (if-exists :supersede))
  (with-open-file (out filepath :direction :output :if-exists if-exists)
    (with-generic-procedure-io-syntax ()
      (pprint (node-serialize (procedure-root pr)) out))))

(defun save-bnode-procedure-to-usds-string (pr)
  (with-generic-procedure-io-syntax ()
    (prin1-to-string (node-serialize (procedure-root pr)))))

(defun read-cnode-procedure-from-file (filepath)
  "CAUTION: Even though READ-EVAL is disabled, relying on READ for data is still dangerous!"
  (with-open-file (in filepath)
    (with-generic-procedure-io-syntax ()
      (read in nil))))

(defun dump-cnode-procedure-to-file (pr filepath &key (if-exists :supersede))
  (with-open-file (out filepath :direction :output :if-exists if-exists)
    (with-generic-procedure-io-syntax ()
      (prin1 pr out))))
