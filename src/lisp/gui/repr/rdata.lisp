;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-viz/repr-rdata
  (:import-from :mopr-viz/repr-shared
                #:multiple-set-c-ref)
  (:use :cl)
  (:export
   #:rdata-command-type
   #:populate-command-from-rdata
   #:rdata
   #:rdata-ynode
   #:frozen-rdata
   #:hidden-rdata
   #:root-container-rdata
   #:expr-container-rdata
   #:expr-label-rdata
   #:expr-input-rdata
   #:content-container-rdata
   #:attr-container-rdata
   #:attr-label-rdata
   #:attr-input-rdata))

(in-package :mopr-viz/repr-rdata)

;;
;;; Utilities
;;

(defun recursive-get-left (ynode)
  (if (autowrap:wrapper-null-p ynode) 0
      (+ (mopr-viz/layout-shared:layout-dimension ynode :left)
         (recursive-get-left (mopr-viz/yoga-fun:node-get-parent ynode)))))

(defun recursive-get-top (ynode)
  (if (autowrap:wrapper-null-p ynode) 0
      (+ (mopr-viz/layout-shared:layout-dimension ynode :top)
         (recursive-get-top (mopr-viz/yoga-fun:node-get-parent ynode)))))

;;
;;; RDATA and Generic Functions
;;

(defgeneric rdata-command-type (node)
  (:documentation "Command type to assign for the node."))

(defgeneric populate-command-from-rdata (node c)
  (:documentation "Populate command to represent the behaviour of the given node."))

(defclass rdata ()
  ((ynode
    :type mopr-viz/yoga-def:node-ref
    :initform (mopr-viz/yoga-fun:node-new)
    :reader rdata-ynode)))

(defmethod initialize-instance :after ((node rdata) &key (yparent nil))
  (when yparent
    (with-slots (ynode) node
      (mopr-viz/yoga-fun:node-insert-child yparent ynode (mopr-viz/yoga-fun:node-get-child-count yparent)))))

(defmethod rdata-command-type ((n rdata))
  mopr-viz/repr-def:+command-type-base+)

(defmethod populate-command-from-rdata ((n rdata) c &aux (y (rdata-ynode n)))
  (multiple-set-c-ref c (mopr-viz/repr-def:combined-command :base)
                      :c-type (rdata-command-type n)
                      :x (recursive-get-left y)
                      :y (recursive-get-top y)
                      :w (mopr-viz/layout-shared:layout-dimension y :width)
                      :h (mopr-viz/layout-shared:layout-dimension y :height)))

;;
;;; FROZEN-RDATA
;;

(defclass frozen-rdata (rdata)
  ())

;;
;;; HIDDEN-RDATA
;;

(defclass hidden-rdata (frozen-rdata)
  ())

(defmethod populate-command-from-rdata ((n hidden-rdata) c)
  (error "Invalid node passed to populate-command-from-rdata!"))

;;
;;; ROOT-CONTAINER-RDATA
;;

(defclass root-container-rdata (frozen-rdata)
  ())

(defmethod initialize-instance :after ((node root-container-rdata) &key)
  (with-slots (ynode) node
    ;; Content rules:
    (mopr-viz/yoga-fun:node-style-set-flex-direction ynode mopr-viz/yoga-def:+flex-direction-column+)
    (mopr-viz/yoga-fun:node-style-set-padding ynode mopr-viz/yoga-def:+edge-all+ 8.0f0)
    (mopr-viz/yoga-fun:node-style-set-gap ynode mopr-viz/yoga-def:+gutter-row+ 8.0f0)))

(defmethod rdata-command-type ((n root-container-rdata))
  mopr-viz/repr-def:+command-type-draw-root-container+)

;;
;;; EXPR-CONTAINER-RDATA
;;

(defclass expr-container-rdata (frozen-rdata)
  ())

(defmethod initialize-instance :after ((node expr-container-rdata) &key)
  (with-slots (ynode) node
    (mopr-viz/yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    ;; Content rules:
    (mopr-viz/yoga-fun:node-style-set-flex-direction ynode mopr-viz/yoga-def:+flex-direction-row+)
    (mopr-viz/yoga-fun:node-style-set-padding ynode mopr-viz/yoga-def:+edge-all+ 6.0f0)
    (mopr-viz/yoga-fun:node-style-set-gap ynode mopr-viz/yoga-def:+gutter-column+ 6.0f0)))

(defmethod rdata-command-type ((n expr-container-rdata))
  mopr-viz/repr-def:+command-type-draw-expr-container+)

;;
;;; EXPR-LABEL-RDATA
;;

(defclass expr-label-rdata (rdata)
  ((bg
    :initarg :bg
    :type mopr-viz/repr-def:command-theme
    :initform mopr-viz/repr-def:+command-theme-none+
    :reader rdata-bg)
   (text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader rdata-text)))

(defmethod initialize-instance :after ((node expr-label-rdata) &key)
  (with-slots (ynode) node
    (mopr-viz/yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    (mopr-viz/yoga-fun:node-style-set-width ynode 60)
    (mopr-viz/yoga-fun:node-style-set-min-height ynode 32)))

(defmethod rdata-command-type ((n expr-label-rdata))
  mopr-viz/repr-def:+command-type-draw-expr-label+)

(defmethod populate-command-from-rdata ((n expr-label-rdata) c)
  (multiple-set-c-ref c (mopr-viz/repr-def:combined-command :draw-expr-label)
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
    (mopr-viz/yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    ;; Content rules:
    (mopr-viz/yoga-fun:node-style-set-flex-direction ynode mopr-viz/yoga-def:+flex-direction-column+)
    (mopr-viz/yoga-fun:node-style-set-padding ynode mopr-viz/yoga-def:+edge-all+ 0.0f0)
    (mopr-viz/yoga-fun:node-style-set-gap ynode mopr-viz/yoga-def:+gutter-row+ 6.0f0)))

;;
;;; ATTR-CONTAINER-RDATA
;;

(defclass attr-container-rdata (hidden-rdata)
  ())

(defmethod initialize-instance :after ((node attr-container-rdata) &key)
  (with-slots (ynode) node
    (mopr-viz/yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    ;; Content rules:
    (mopr-viz/yoga-fun:node-style-set-flex-direction ynode mopr-viz/yoga-def:+flex-direction-row+)
    (mopr-viz/yoga-fun:node-style-set-padding ynode mopr-viz/yoga-def:+edge-all+ 0.0f0)
    (mopr-viz/yoga-fun:node-style-set-gap ynode mopr-viz/yoga-def:+gutter-column+ 4.0f0)))

(defclass attr-label-rdata (rdata)
  ((bg
    :initarg :bg
    :type mopr-viz/repr-def:command-theme
    :initform mopr-viz/repr-def:+command-theme-none+
    :reader rdata-bg)
   (text
    :initarg :text
    :type base-string
    :initform (make-string 0 :element-type 'base-char)
    :reader rdata-text)))

(defmethod initialize-instance :after ((node attr-label-rdata) &key (h-co 1))
  (with-slots (ynode text) node
    (mopr-viz/yoga-fun:node-style-set-flex-grow ynode 0.0f0)
    (mopr-viz/yoga-fun:node-style-set-min-width ynode (+ 16 (* (length text) 10)))
    (mopr-viz/yoga-fun:node-style-set-min-height ynode (+ 16 (* h-co 16)))))

(defmethod rdata-command-type ((n attr-label-rdata))
  mopr-viz/repr-def:+command-type-draw-attr-label+)

(defmethod populate-command-from-rdata ((n attr-label-rdata) c)
  (multiple-set-c-ref c (mopr-viz/repr-def:combined-command :draw-attr-label)
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
    (mopr-viz/yoga-fun:node-style-set-flex-grow ynode 1.0f0)
    (mopr-viz/yoga-fun:node-style-set-min-width ynode 200)
    (mopr-viz/yoga-fun:node-style-set-min-height ynode (+ 16 (* h-co 16)))))

(defmethod rdata-command-type ((n attr-input-rdata))
  mopr-viz/repr-def:+command-type-draw-attr-input+)

(defmethod populate-command-from-rdata ((n attr-input-rdata) c)
  (multiple-set-c-ref c (mopr-viz/repr-def:combined-command :draw-attr-input)
                      :text (autowrap:alloc-string (rdata-text n)))
  (call-next-method))
