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
            (:constructor make-cnode-procedure-from-usds-file
                (filepath read-pkg &aux (root (read-from-usds-file filepath read-pkg)))))
  (header (make-header)
   :type header)
  (root nil
   :type (or null cnode)))

(defun procedure-apply-to-layer (pr layer-h call-enabled
                                 &aux (cn (mopr-sgt:procedure-root pr)))
  (unless (zerop (mopr:layer-try-upgrade layer-h))
    (mopr:with-handle (stage-h :stage)
      (mopr:stage-open-layer stage-h layer-h)
      (mopr-info:with-registry (:supported-cases '(:upcase))
        ;; (cnode-debug-print cn)
        (let* ((preprocess-all-fn (if call-enabled
                                      #'preprocess-all-call-enabled
                                      #'preprocess-all))
               (cn-preprocessed (funcall preprocess-all-fn cn)))
          ;; (cnode-debug-print cn-preprocessed)
          (with-execution-variables ()
            (cnode-execute cn-preprocessed stage-h)))))))
