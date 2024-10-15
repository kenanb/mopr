;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr
  (:import-from :mopr)
  (:import-from :plus-c) ; Depend on system.
  (:import-from :float-features) ; Depend on system.
  (:use :cl)
  (:export
   #:populate-command-queue
   #:destruct-command-queue
   #:with-layout-settings
   #:testing))

(in-package :mopr-ext/repr)

(defvar *fill-column* 70)

(defvar *debug-mode* t)

(defvar *enable-call* nil)

;;
;;; Utilities
;;

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

(defun format-form (form margin)
  (let ((*print-pretty* t)
        (*print-right-margin* margin))
    (let* ((text (format nil "~S" form))
           (line-count (1+ (count #\newline text))))
      (values text line-count))))

(defmacro %dim (ynode accessor)
  (case accessor
    (:left `(yoga-fun:node-layout-get-left ,ynode))
    (:right `(yoga-fun:node-layout-get-right ,ynode))
    (:top `(yoga-fun:node-layout-get-top ,ynode))
    (:bottom `(yoga-fun:node-layout-get-bottom ,ynode))
    (:width `(yoga-fun:node-layout-get-width ,ynode))
    (:height `(yoga-fun:node-layout-get-height ,ynode))))

(defun recursive-get-left (ynode)
  (if (autowrap:wrapper-null-p ynode) 0
      (+ (%dim ynode :left)
         (recursive-get-left (yoga-fun:node-get-parent ynode)))))

(defun recursive-get-top (ynode)
  (if (autowrap:wrapper-null-p ynode) 0
      (+ (%dim ynode :top)
         (recursive-get-top (yoga-fun:node-get-parent ynode)))))

(defmacro %set-values (obj accessor &rest key-value-plist)
  `(progn
     ,@(loop for (k v . rest) on key-value-plist by #'cddr
             collecting `(setf (plus-c:c-ref ,obj ,@accessor ,k) ,v))))

;;
;;; RDATA and Generic Functions
;;

;; Zero value is reserved for "no selection".
(defvar *rdata-id-counter* 1)

(defgeneric rdata-command-type (node)
  (:documentation "Command type to assign for the node."))

(defgeneric populate-command-from-rdata (node c)
  (:documentation "Populate command to represent the behaviour of the given node."))

(defclass rdata ()
  ((id
    :type (unsigned-byte 32)
    :initform (prog1 *rdata-id-counter* (incf *rdata-id-counter*))
    :reader rdata-id)
   (ynode
    :type yoga-def:node-ref
    :initform (yoga-fun:node-new)
    :reader rdata-ynode)))

(defmethod initialize-instance :after ((node rdata) &key (yparent nil))
  (when yparent
    (with-slots (ynode) node
      (yoga-fun:node-insert-child yparent ynode (yoga-fun:node-get-child-count yparent)))))

(defmethod rdata-command-type ((n rdata))
  mopr-def:+command-type-base+)

(defmethod populate-command-from-rdata ((n rdata) c &aux (y (rdata-ynode n)))
  (%set-values c (mopr-def:combined-command :base)
               :c-type (rdata-command-type n)
               :id (rdata-id n)
               :x (recursive-get-left y)
               :y (recursive-get-top y)
               :w (%dim y :width)
               :h (%dim y :height)))

;;
;;; HIDDEN-RDATA
;;

(defclass hidden-rdata (rdata)
  ())

(defmethod populate-command-from-rdata ((n hidden-rdata) c)
  (error "Invalid node passed to populate-command-from-rdata!"))

;;
;;; ROOT-CONTAINER-RDATA
;;

(defclass root-container-rdata (rdata)
  ())

(defmethod initialize-instance :after ((node root-container-rdata) &key)
  (with-slots (ynode) node
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-column+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 8.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-row+ 8.0f0)))

(defmethod rdata-command-type ((n root-container-rdata))
  mopr-def:+command-type-draw-root-container+)

;;
;;; EXPR-CONTAINER-RDATA
;;

(defclass expr-container-rdata (rdata)
  ())

(defmethod initialize-instance :after ((node expr-container-rdata) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-row+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 6.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-column+ 6.0f0)))

(defmethod rdata-command-type ((n expr-container-rdata))
  mopr-def:+command-type-draw-expr-container+)

;;
;;; EXPR-LABEL-RDATA
;;

(defclass expr-label-rdata (rdata)
  ((bg
    :initarg :bg
    :type mopr-def:command-theme
    :initform mopr-def:+command-theme-none+
    :reader rdata-bg)
   (text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader rdata-text)))

(defmethod initialize-instance :after ((node expr-label-rdata) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    (yoga-fun:node-style-set-width ynode 60)
    (yoga-fun:node-style-set-min-height ynode 32)))

(defmethod rdata-command-type ((n expr-label-rdata))
  mopr-def:+command-type-draw-expr-label+)

(defmethod populate-command-from-rdata ((n expr-label-rdata) c)
  (%set-values c (mopr-def:combined-command :draw-expr-label)
               :bg (rdata-bg n)
               :text (autowrap:alloc-string (rdata-text n)))
  (call-next-method))

;;
;;; CONTENT-CONTAINER-RDATA
;;

(defclass content-container-rdata (hidden-rdata)
  ())

(defmethod initialize-instance :after ((node content-container-rdata) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-column+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 0.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-row+ 6.0f0)))

;;
;;; ATTR-CONTAINER-RDATA
;;

(defclass attr-container-rdata (hidden-rdata)
  ())

(defmethod initialize-instance :after ((node attr-container-rdata) &key)
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    ;; Content rules:
    (yoga-fun:node-style-set-flex-direction ynode yoga-def:+flex-direction-row+)
    (yoga-fun:node-style-set-padding ynode yoga-def:+edge-all+ 0.0f0)
    (yoga-fun:node-style-set-gap ynode yoga-def:+gutter-column+ 4.0f0)))

(defclass attr-label-rdata (rdata)
  ((bg
    :initarg :bg
    :type mopr-def:command-theme
    :initform mopr-def:+command-theme-none+
    :reader rdata-bg)
   (text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader rdata-text)))

(defmethod initialize-instance :after ((node attr-label-rdata) &key (h-co 1))
  (with-slots (ynode text) node
    (yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    (yoga-fun:node-style-set-min-width ynode (+ 16 (* (length text) 10)))
    (yoga-fun:node-style-set-min-height ynode (+ 16 (* h-co 16)))))

(defmethod rdata-command-type ((n attr-label-rdata))
  mopr-def:+command-type-draw-attr-label+)

(defmethod populate-command-from-rdata ((n attr-label-rdata) c)
  (%set-values c (mopr-def:combined-command :draw-attr-label)
               :bg (rdata-bg n)
               :text (autowrap:alloc-string (rdata-text n)))
  (call-next-method))

(defclass attr-input-rdata (rdata)
  ((text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader rdata-text)))

(defmethod initialize-instance :after ((node attr-input-rdata) &key (h-co 1))
  (with-slots (ynode) node
    (yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    (yoga-fun:node-style-set-min-width ynode 200)
    (yoga-fun:node-style-set-min-height ynode (+ 16 (* h-co 16)))))

(defmethod rdata-command-type ((n attr-input-rdata))
  mopr-def:+command-type-draw-attr-input+)

(defmethod populate-command-from-rdata ((n attr-input-rdata) c)
  (%set-values c (mopr-def:combined-command :draw-attr-input)
               :text (autowrap:alloc-string (rdata-text n)))
  (call-next-method))

;;
;;; Form Handlers
;;

(defun handle-var-form (yparent form
                        &aux
                          (var-name (car form))
                          (var-aux (cadr form))
                          (frest (cddr form)))
  ;; (format t "~%Called handle-var-form!~%: ~S~%" form)
  (multiple-value-bind (text-body line-count-body)
      (format-form frest *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-0+)
           (nec (make-instance 'expr-container-rdata :yparent yparent))
           (nel (make-instance 'expr-label-rdata
                               :yparent (rdata-ynode nec)
                               :text "VAR"
                               :bg color))
           (ncc (make-instance 'content-container-rdata
                               :yparent (rdata-ynode nec)))
           (nac0 (make-instance 'attr-container-rdata
                                :yparent (rdata-ynode ncc)))
           (nal0 (make-instance 'attr-label-rdata
                                :yparent (rdata-ynode nac0)
                                :text "NAME"
                                :bg color))
           (nai0 (make-instance 'attr-input-rdata
                                :yparent (rdata-ynode nac0)
                                :text (format nil "~S" var-name)))
           (nac1 (make-instance 'attr-container-rdata
                                :yparent (rdata-ynode ncc)))
           (nal1 (make-instance 'attr-label-rdata
                                :yparent (rdata-ynode nac1)
                                :text "LET"
                                :bg color))
           (nai1 (make-instance 'attr-input-rdata
                                :yparent (rdata-ynode nac1)
                                :text (format nil "~S" var-aux)))
           (nar (make-instance 'attr-input-rdata
                               :yparent (rdata-ynode ncc)
                               :text text-body
                               :h-co line-count-body)))
      (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nar))))

(defun handle-each-form (yparent form
                         &aux
                           (name (car form))
                           (arg-aggrs (cadr form))
                           (val-aggrs (cddr form)))
  ;; (format t "~%Called handle-each-form!~%: ~S~%" form)
  (multiple-value-bind (text-val-aggrs line-count-val-aggrs)
      (format-form val-aggrs *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-1+)
           (nec (make-instance 'expr-container-rdata :yparent yparent))
           (nel (make-instance 'expr-label-rdata
                               :yparent (rdata-ynode nec)
                               :text "EACH"
                               :bg color))
           (ncc (make-instance 'content-container-rdata
                               :yparent (rdata-ynode nec)))
           (nac0 (make-instance 'attr-container-rdata
                                :yparent (rdata-ynode ncc)))
           (nal0 (make-instance 'attr-label-rdata
                                :yparent (rdata-ynode nac0)
                                :text "NAME"
                                :bg color))
           (nai0 (make-instance 'attr-input-rdata
                                :yparent (rdata-ynode nac0)
                                :text (format nil "~S" name)))
           (nac1 (make-instance 'attr-container-rdata
                                :yparent (rdata-ynode ncc)))
           (nal1 (make-instance 'attr-label-rdata
                                :yparent (rdata-ynode nac1)
                                :text "ARG-AGGR(S)"
                                :bg color))
           (nai1 (make-instance 'attr-input-rdata
                                :yparent (rdata-ynode nac1)
                                :text (format nil "~S" arg-aggrs)))
           (nac2 (make-instance 'attr-container-rdata
                                :yparent (rdata-ynode ncc)))
           (nal2 (make-instance 'attr-label-rdata
                                :yparent (rdata-ynode nac2)
                                :text "VAL-AGGR(S)"
                                :bg color))
           (nai2 (make-instance 'attr-input-rdata
                                :yparent (rdata-ynode nac2)
                                :text (format nil "~S" val-aggrs))))
      (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nac2 nal2 nai2))))

(defun handle-iota-form (yparent form
                         &aux
                           (name (car form))
                           (arg-aggr (cadr form))
                           (val-aggr (caddr form)))
  ;; (format t "~%Called handle-iota-form!~%: ~S~%" form)
  (let* ((color mopr-def:+command-theme-expr-bg-2+)
         (nec (make-instance 'expr-container-rdata :yparent yparent))
         (nel (make-instance 'expr-label-rdata
                             :yparent (rdata-ynode nec)
                             :text "IOTA"
                             :bg color))
         (ncc (make-instance 'content-container-rdata
                             :yparent (rdata-ynode nec)))
         (nac0 (make-instance 'attr-container-rdata
                              :yparent (rdata-ynode ncc)))
         (nal0 (make-instance 'attr-label-rdata
                              :yparent (rdata-ynode nac0)
                              :text "NAME"
                              :bg color))
         (nai0 (make-instance 'attr-input-rdata
                              :yparent (rdata-ynode nac0)
                              :text (format nil "~S" name)))
         (nac1 (make-instance 'attr-container-rdata
                              :yparent (rdata-ynode ncc)))
         (nal1 (make-instance 'attr-label-rdata
                              :yparent (rdata-ynode nac1)
                              :text "ARG-AGGR"
                              :bg color))
         (nai1 (make-instance 'attr-input-rdata
                              :yparent (rdata-ynode nac1)
                              :text (format nil "~S" arg-aggr)))
         (nac2 (make-instance 'attr-container-rdata
                              :yparent (rdata-ynode ncc)))
         (nal2 (make-instance 'attr-label-rdata
                              :yparent (rdata-ynode nac2)
                              :text "VAL-AGGR"
                              :bg color))
         (nai2 (make-instance 'attr-input-rdata
                              :yparent (rdata-ynode nac2)
                              :text (format nil "~S" val-aggr))))
    (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1 nac2 nal2 nai2)))

(defun handle-call-form (yparent form
                         &aux
                           (call-aux (car form))
                           (call-body (cdr form)))
  ;; (format t "~%Called handle-call-form!~%: ~S~%" form)
  (multiple-value-bind (text-call-body line-count-call-body)
      (format-form call-body *fill-column*)
    (let* ((color mopr-def:+command-theme-expr-bg-3+)
           (nec (make-instance 'expr-container-rdata :yparent yparent))
           (nel (make-instance 'expr-label-rdata
                               :yparent (rdata-ynode nec)
                               :text "CALL"
                               :bg color))
           (ncc (make-instance 'content-container-rdata
                               :yparent (rdata-ynode nec)))
           (nac0 (make-instance 'attr-container-rdata
                                :yparent (rdata-ynode ncc)))
           (nal0 (make-instance 'attr-label-rdata
                                :yparent (rdata-ynode nac0)
                                :text "LET"
                                :bg color))
           (nai0 (make-instance 'attr-input-rdata
                                :yparent (rdata-ynode nac0)
                                :text (format nil "~S" call-aux)))
           (nar (make-instance 'attr-input-rdata
                               :yparent (rdata-ynode ncc)
                               :text text-call-body
                               :h-co line-count-call-body)))
      (list nec nel ncc nac0 nal0 nai0 nar))))

(defun handle-prim-form (yparent form
                         &aux
                           (fpath (car form))
                           (fmeta (cadr form))
                           (frest (cddr form)))
  ;; (format t "~%Called handle-prim-form!~%: ~S~%" form)
  (let* ((color mopr-def:+command-theme-expr-bg-4+)
         (nec (make-instance 'expr-container-rdata :yparent yparent))
         (nel (make-instance 'expr-label-rdata
                             :yparent (rdata-ynode nec)
                             :text "PRIM"
                             :bg color))
         (ncc (make-instance 'content-container-rdata
                             :yparent (rdata-ynode nec)))
         (nac0 (make-instance 'attr-container-rdata
                              :yparent (rdata-ynode ncc)))
         (nal0 (make-instance 'attr-label-rdata
                              :yparent (rdata-ynode nac0)
                              :text "PATH"
                              :bg color))
         (nai0 (make-instance 'attr-input-rdata
                              :yparent (rdata-ynode nac0)
                              :text (format nil "~S" fpath)))
         (nac1 (make-instance 'attr-container-rdata
                              :yparent (rdata-ynode ncc)))
         (nal1 (make-instance 'attr-label-rdata
                              :yparent (rdata-ynode nac1)
                              :text "META"
                              :bg color))
         (nai1 (make-instance 'attr-input-rdata
                              :yparent (rdata-ynode nac1)
                              :text (format nil "~S" fmeta)))
         (nested-rdatas
           (loop for l in frest
                 for i from 0
                 for fn = (case (car l)
                            ;; TODO : Handle other forms.
                            (:call   #'handle-call-form)
                            (:|call| #'handle-call-form))
                 nconc (if fn
                           (funcall fn (rdata-ynode ncc) (cdr l))
                           (unknown-form-error (car l) :debug)))))
    (concatenate 'list (list nec nel ncc nac0 nal0 nai0 nac1 nal1 nai1) nested-rdatas)))

(defun handle-tree-form (yparent form)
  ;; (format t "~%Called handle-tree-form!~%: ~S~%" form)
  (let* ((color mopr-def:+command-theme-expr-bg-5+)
         (nec (make-instance 'expr-container-rdata :yparent yparent))
         (nel (make-instance 'expr-label-rdata
                             :yparent (rdata-ynode nec)
                             :text "TREE"
                             :bg color))
         (ncc (make-instance 'content-container-rdata
                             :yparent (rdata-ynode nec)))
         (nar (make-instance 'attr-input-rdata
                             :yparent (rdata-ynode ncc)
                             :text (format-form form *fill-column*))))
    (list nec nel ncc nar)))

(defun handle-meta-form (yparent form)
  ;; (format t "~%Called handle-meta-form!~%: ~S~%" form)
  (declare (ignore form))
  (let* ((color mopr-def:+command-theme-expr-bg-6+)
         (nec (make-instance 'expr-container-rdata :yparent yparent))
         (nel (make-instance 'expr-label-rdata
                             :yparent (rdata-ynode nec)
                             :text "META"
                             :color color))
         (ncc (make-instance 'content-container-rdata
                             :yparent (rdata-ynode nec)))
         ;; TODO : Add support for metadata handling.
         )
    (list nec nel ncc)))

(defun handle-data-subforms (yparent subforms)
  ;; (format t "~%Called handle-data-subforms!~%: ~S~%" subforms)
  (loop for l in subforms
        for i from 0
        for fn = (case (car l)
                   (:var    #'handle-var-form)
                   (:|var|  #'handle-var-form)
                   (:each   #'handle-each-form)
                   (:|each| #'handle-each-form)
                   (:iota   #'handle-iota-form)
                   (:|iota| #'handle-iota-form)
                   (:call   #'handle-call-form)
                   (:|call| #'handle-call-form)
                   (:meta   #'handle-meta-form)
                   (:|meta| #'handle-meta-form)
                   (:tree   #'handle-tree-form)
                   (:|tree| #'handle-tree-form)
                   (:prim   #'handle-prim-form)
                   (:|prim| #'handle-prim-form))
        nconc (if fn
                  (funcall fn yparent (cdr l))
                  (unknown-form-error (car l) :debug))))

;;
;;; Top-Level API and Macros
;;

(defmacro with-repr-variables ((&key
                                  (enable-call nil))
                               &body body)
  `(let* ((*enable-call* ,enable-call))
     ,@body))

(defun build-repr-call-enabled (usds-data)
  (mopr-info:with-registry (:supported-cases '(:upcase))
    (mopr-plug:with-configuration ()
      (with-repr-variables (:enable-call t)
        (let* ((root-rdata (make-instance 'root-container-rdata)))
          (cons root-rdata (handle-data-subforms (rdata-ynode root-rdata) usds-data)))))))

(defmacro with-layout-settings (&body body)
  `(float-features:with-float-traps-masked (:invalid)
     ,@body))

(defun populate-command-queue (cmd-queue usds-data)
  (with-layout-settings
      (let* ((pixels-w (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-w))
             ;; (pixels-h (plus-c:c-ref cmd-queue mopr-def:command-queue :pixels-h))
             (rdatas (build-repr-call-enabled usds-data))
             (visible-rdatas (remove-if (lambda (x) (typep x 'hidden-rdata)) rdatas))
             (cmd-count (length visible-rdatas))
             (ynode (rdata-ynode (car visible-rdatas)))
             (commands (autowrap:alloc 'mopr-def:combined-command cmd-count)))

        (yoga-fun:node-calculate-layout ynode
                                        pixels-w
                                        yoga-def:+undefined+ ;; pixels-h
                                        yoga-def:+direction-ltr+)

        (loop for n in visible-rdatas
              for i to cmd-count
              do (populate-command-from-rdata
                  n
                  (autowrap:c-aref commands i 'mopr-def:combined-command)))

        (%set-values cmd-queue (mopr-def:command-queue)
                     :nof-commands cmd-count
                     :commands (autowrap:ptr commands)
                     ;; Adjust height to the actual "used" height.
                     :pixels-h (%dim ynode :height))

        (yoga-fun:node-free-recursive ynode))))

;; NOTE: Free calls are made from the same module the allocations were made from,
;;       to avoid possible issues with multiple malloc implementations in runtime.
(defun destruct-command-queue (cmd-queue-ptr
                               &aux
                                 (cmd-queue (autowrap:wrap-pointer
                                             cmd-queue-ptr 'mopr-def:command-queue)))
  (let* ((cmd-count (plus-c:c-ref cmd-queue mopr-def:command-queue :nof-commands))
         (commands (plus-c:c-ref cmd-queue mopr-def:command-queue :commands)))

    (loop for i to cmd-count
          for c = (autowrap:c-aref commands i 'mopr-def:combined-command)
          for c-type = (plus-c:c-ref c mopr-def:combined-command :base :c-type)

          ;; TODO: Formalize cleanup.
          ;;
          ;; NOTE: c-ref implements some convenience functionality based on the
          ;; last field.  However, in order to free the C string, we do need the
          ;; pointer.  So we dereference and reference in the end, to inhibit
          ;; foreign-string last-field convenience.
          do (case c-type
               (mopr-def:+command-type-draw-expr-label+
                (autowrap::free (plus-c:c-ref c mopr-def:combined-command
                                              :draw-expr-label
                                              :text plus-c:* plus-c:&)))
               (mopr-def:+command-type-draw-attr-label+
                (autowrap::free (plus-c:c-ref c mopr-def:combined-command
                                              :draw-attr-label
                                              :text plus-c:* plus-c:&)))
               (mopr-def:+command-type-draw-attr-input+
                (autowrap::free (plus-c:c-ref c mopr-def:combined-command
                                              :draw-attr-input
                                              :text plus-c:* plus-c:&)))))

    (autowrap:free commands)

    (%set-values cmd-queue (mopr-def:command-queue) :nof-commands 0 :commands (autowrap:ptr nil))))

;;
;;; TESTING
;;

(defun dummy-yoga-layout (pixels-w pixels-h)
  ;; For disabling pixel rounding:
  ;; YGConfigRef config = YGConfigNew( );
  ;; YGConfigSetPointScaleFactor( config, 0.0f );
  (let ((root (yoga-fun:node-new))
        (c0 (yoga-fun:node-new))
        (c1 (yoga-fun:node-new))
        (c1c0 (yoga-fun:node-new))
        (c1c1 (yoga-fun:node-new))
        (c1c2 (yoga-fun:node-new)))
    (yoga-fun:node-style-set-flex-direction root yoga-def:+flex-direction-column+)
    (yoga-fun:node-style-set-width root pixels-w)
    (yoga-fun:node-style-set-height root pixels-h)
    (yoga-fun:node-style-set-padding root yoga-def:+edge-all+ 10.0f0)

    (yoga-fun:node-style-set-flex-grow c0 1.0f0)
    (yoga-fun:node-style-set-margin c0 yoga-def:+edge-all+ 10.0f0)

    (yoga-fun:node-style-set-flex-grow c1 1.0f0)
    (yoga-fun:node-style-set-margin c1 yoga-def:+edge-all+ 10.0f0)

    (yoga-fun:node-style-set-flex-direction c1 yoga-def:+flex-direction-row+)

    (yoga-fun:node-style-set-flex-grow c1c0 1.0f0)
    (yoga-fun:node-style-set-margin c1c0 yoga-def:+edge-all+ 10.0f0)
    (yoga-fun:node-style-set-flex-grow c1c1 1.0f0)
    (yoga-fun:node-style-set-margin c1c1 yoga-def:+edge-all+ 10.0f0)
    (yoga-fun:node-style-set-flex-grow c1c2 1.0f0)
    (yoga-fun:node-style-set-margin c1c2 yoga-def:+edge-all+ 10.0f0)

    (yoga-fun:node-insert-child root c0 0)
    (yoga-fun:node-insert-child root c1 0)
    (yoga-fun:node-insert-child c1 c1c0 0)
    (yoga-fun:node-insert-child c1 c1c1 1)
    (yoga-fun:node-insert-child c1 c1c2 2)

    ;; (yoga-fun:node-calculate-layout root pixels-w pixels-h yoga-def:+direction-ltr+))
    (yoga-fun:node-calculate-layout root
                                    yoga-def:+undefined+
                                    yoga-def:+undefined+
                                    yoga-def:+direction-ltr+)

    (prog1 (%dim c1 :top)
      ;; (format t "~{~a~}"
      ;;         (list
      ;;          (list (%dim c0 :left)
      ;;                (%dim c0 :top))
      ;;          (list (+ (%dim c0 :left) (%dim c0 :width))
      ;;                (+ (%dim c0 :top) (%dim c0 :height)))))
      (yoga-fun:node-free-recursive root))))

(defun testing ()
  (with-layout-settings
      (dummy-yoga-layout 640.0f0 480.0f0)))
