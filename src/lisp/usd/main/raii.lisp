(in-package #:mopr-usd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-create-fn-symbol (x)
    (alexandria:format-symbol "MOPR-USD" "CREATE-~A" x))

  (defun get-delete-fn-symbol (x)
    (alexandria:format-symbol "MOPR-USD" "DELETE-~A" x)))

;; NOTE: Since the variables are lexically scoped,
;; we don't bother calling (autowrap:invalidate stage) at scope exit.

(defmacro with-handle ((name type) &body body)
  `(let ((,name (,(get-create-fn-symbol type))))
     (unwind-protect (progn ,@body)
       (,(get-delete-fn-symbol type) ,name))))

(defmacro with-handles ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (bind)
                     `(,(car bind) (,(get-create-fn-symbol (cadr bind)))))
          bindings)
     (unwind-protect (progn ,@body)
       ,@(reverse (mapcar #'(lambda (bind)
                              `(,(get-delete-fn-symbol (cadr bind)) ,(car bind)))
                          bindings)))))

(defmacro with-handles* ((&rest bindings) &body body)
  `(let* ,(mapcar #'(lambda (bind)
                      `(,(car bind) (,(get-create-fn-symbol (cadr bind)))))
           bindings)
     (unwind-protect (progn ,@body)
       ,@(reverse (mapcar #'(lambda (bind)
                              `(,(get-delete-fn-symbol (cadr bind)) ,(car bind)))
                          bindings)))))
