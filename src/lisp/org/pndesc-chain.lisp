;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-org)

;; See: MOPR-URI:DESC-CHAIN. Besides participating in URIs, PNDESCRIPTOR is also
;; used to refer to filesystem paths.

(defun %rchain-pndescriptor-paths (pndescriptors &key file-expected-p
                                   &aux
                                     (p (pndescriptor-path (car pndescriptors)))
                                     (r (cdr pndescriptors)))
  (cond
    ((and file-expected-p (directory-pathname-p p))
     (error "Directory path found while a file path was expected in pndescriptor!"))
    ((and (not file-expected-p) (file-pathname-p p))
     (error "File path found while a directory path was expected in pndescriptor!"))
    ((and r (absolute-pathname-p p))
     (error "Non-terminating pndescriptor path cannot be absolute!"))
    (r (merge-pathnames* p (%rchain-pndescriptor-paths r)))
    (t p)))

(defun desc-chain-as-path (chain &key file-expected-p relative-expected-p)
  "DESC-CHAIN-AS-PATH

Joins the paths of each pndescriptor. EXAMPLE: ( '/a' , 'b/c/d' , 'e/f' ) => /a/b/c/d/e/f
"
  (let ((result (%rchain-pndescriptor-paths (reverse (mopr-uri:desc-chain-contents chain))
                                            :file-expected-p file-expected-p)))
    (cond
      ((and relative-expected-p (absolute-pathname-p result))
       (error "Descriptor chaining resulted in absolute path, while relative path was expected!"))
      ((and (not relative-expected-p) (relative-pathname-p result))
       (error "Descriptor chaining resulted in relative path, while absolute path was expected!"))
      (t result))))
