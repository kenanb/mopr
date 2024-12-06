;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (procedure
            (:copier nil)
            (:constructor)
            (:constructor make-cnode-procedure
                (pr &aux (root (cnode-from-node-recursive (procedure-root pr)))))
            (:constructor make-enode-procedure
                (pr &aux (root (enode-from-node-recursive (procedure-root pr)))))
            (:constructor make-preprocessed-cnode-procedure
                (pr call-enabled &aux (preprocess-all-fn (if call-enabled
                                                             #'preprocess-all-call-enabled
                                                             #'preprocess-all))
                                   (root (funcall preprocess-all-fn (procedure-root pr)))))
            (:constructor make-cnode-procedure-from-usds-file
                (filepath read-pkg &aux (root (read-from-usds-file filepath read-pkg)))))
  (header (make-header)
   :type header)
  (root nil
   :type (or null cnode)))

(defun procedure-debug-print (pr)
  (cnode-debug-print (procedure-root pr)))

(defun procedure-apply-to-layer (pr layer-h call-enabled)
  (unless (zerop (mopr:layer-try-upgrade layer-h))
    (mopr:with-handle (stage-h :stage)
      (mopr:stage-open-layer stage-h layer-h)
      (mopr-info:with-registry (:supported-cases '(:upcase))
        ;; (procedure-debug-print pr)
        (let* ((pr-preprocessed (make-preprocessed-cnode-procedure pr call-enabled)))
          ;; (procedure-debug-print pr)
          (with-execution-variables ()
            (cnode-execute (procedure-root pr-preprocessed) stage-h)))))))

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

(defun save-cnode-procedure-to-usds-file (pr filepath &key (if-exists :supersede))
  (with-open-file (out filepath :direction :output :if-exists if-exists)
    (with-generic-procedure-io-syntax ()
      (pprint (cnode-serialize (procedure-root pr)) out))))

(defun save-cnode-procedure-to-usds-string (pr)
  (with-generic-procedure-io-syntax ()
    (prin1-to-string (cnode-serialize (procedure-root pr)))))

(defun read-cnode-procedure-from-file (filepath)
  "CAUTION: Even though READ-EVAL is disabled, relying on READ for data is still dangerous!"
  (with-open-file (in filepath)
    (with-generic-procedure-io-syntax ()
      (read in nil))))

(defun dump-cnode-procedure-to-file (pr filepath &key (if-exists :supersede))
  (with-open-file (out filepath :direction :output :if-exists if-exists)
    (with-generic-procedure-io-syntax ()
      (prin1 pr out))))
