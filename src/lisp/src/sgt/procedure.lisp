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
