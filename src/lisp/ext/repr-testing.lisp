;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package :cl-user)

(defpackage :mopr-ext/repr-testing
  (:import-from :mopr)
  (:import-from :mopr-ext/repr-shared
                #:layout-dimension
                #:with-layout-settings)
  (:use :cl)
  (:export
   #:testing))

(in-package :mopr-ext/repr-testing)

(defun dummy-yoga-layout (pixels-w pixels-h)
  ;; For disabling pixel rounding:
  ;; YGConfigRef config = YGConfigNew( );
  ;; YGConfigSetPointScaleFactor( config, 0.0f );
  (let ((root (mopr-gui/yoga-fun:node-new))
        (c0 (mopr-gui/yoga-fun:node-new))
        (c1 (mopr-gui/yoga-fun:node-new))
        (c1c0 (mopr-gui/yoga-fun:node-new))
        (c1c1 (mopr-gui/yoga-fun:node-new))
        (c1c2 (mopr-gui/yoga-fun:node-new)))
    (mopr-gui/yoga-fun:node-style-set-flex-direction root mopr-gui/yoga-def:+flex-direction-column+)
    (mopr-gui/yoga-fun:node-style-set-width root pixels-w)
    (mopr-gui/yoga-fun:node-style-set-height root pixels-h)
    (mopr-gui/yoga-fun:node-style-set-padding root mopr-gui/yoga-def:+edge-all+ 10.0f0)

    (mopr-gui/yoga-fun:node-style-set-flex-grow c0 1.0f0)
    (mopr-gui/yoga-fun:node-style-set-margin c0 mopr-gui/yoga-def:+edge-all+ 10.0f0)

    (mopr-gui/yoga-fun:node-style-set-flex-grow c1 1.0f0)
    (mopr-gui/yoga-fun:node-style-set-margin c1 mopr-gui/yoga-def:+edge-all+ 10.0f0)

    (mopr-gui/yoga-fun:node-style-set-flex-direction c1 mopr-gui/yoga-def:+flex-direction-row+)

    (mopr-gui/yoga-fun:node-style-set-flex-grow c1c0 1.0f0)
    (mopr-gui/yoga-fun:node-style-set-margin c1c0 mopr-gui/yoga-def:+edge-all+ 10.0f0)
    (mopr-gui/yoga-fun:node-style-set-flex-grow c1c1 1.0f0)
    (mopr-gui/yoga-fun:node-style-set-margin c1c1 mopr-gui/yoga-def:+edge-all+ 10.0f0)
    (mopr-gui/yoga-fun:node-style-set-flex-grow c1c2 1.0f0)
    (mopr-gui/yoga-fun:node-style-set-margin c1c2 mopr-gui/yoga-def:+edge-all+ 10.0f0)

    (mopr-gui/yoga-fun:node-insert-child root c0 0)
    (mopr-gui/yoga-fun:node-insert-child root c1 0)
    (mopr-gui/yoga-fun:node-insert-child c1 c1c0 0)
    (mopr-gui/yoga-fun:node-insert-child c1 c1c1 1)
    (mopr-gui/yoga-fun:node-insert-child c1 c1c2 2)

    ;; (mopr-gui/yoga-fun:node-calculate-layout root pixels-w pixels-h mopr-gui/yoga-def:+direction-ltr+))
    (mopr-gui/yoga-fun:node-calculate-layout root
                                             mopr-gui/yoga-def:+undefined+
                                             mopr-gui/yoga-def:+undefined+
                                             mopr-gui/yoga-def:+direction-ltr+)

    (prog1 (layout-dimension c1 :top)
      ;; (format t "~{~a~}"
      ;;         (list
      ;;          (list (layout-dimension c0 :left)
      ;;                (layout-dimension c0 :top))
      ;;          (list (+ (layout-dimension c0 :left) (layout-dimension c0 :width))
      ;;                (+ (layout-dimension c0 :top) (layout-dimension c0 :height)))))
      (mopr-gui/yoga-fun:node-free-recursive root))))

(defun testing ()
  (with-layout-settings
      (dummy-yoga-layout 640.0f0 480.0f0)))
