;;;; package.lisp

(defpackage :mopr-sgt
  (:use #:cl)
  (:export
   #:data-group
   #:tree-entry
   #:prim-entry
   #:prop-entry
   #:make-data-group
   #:make-group
   #:make-tree-entry
   #:make-prim-entry
   #:make-prop-entry
   #:make-prop
   #:data-group-data
   #:tree-entry-data
   #:prim-entry-data
   #:prop-entry-data
   #:prop-entry-info))
