;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defstruct (pndescriptor
            (:include mopr-uri:descriptor)
            (:constructor)
            (:constructor make-new-pndescriptor
                (role path &aux (uuid (mopr-uri:new-uuid path)))))
  "PATHNAME DESCRIPTOR

A pndescriptor represents the means to unambiguously refer to a resource or
resource grouping that is directly associated with a pathname.

Every entity that has an associated descriptor will be associated with a UUID at
creation-time. Normally, this will be a UUIDv7, so that the underlying content
can be moved while maintaining stable addressing by the client application. But
it will be possible to configure the UUID version to enable deterministic
addressing. TODO: Implement setting.
"
  (path (error "PNDESCRIPTOR cannot be initialized without a path!")
   :type pathname
   :read-only t))

(defun make-pndescriptor-for-file (role file)
  (make-new-pndescriptor
   role
   (or (file-pathname-p file)
       (error "File descriptor requested for non-file path!"))))

(defun make-pndescriptor-for-directory (role directory)
  (make-new-pndescriptor
   role
   (ensure-directory-pathname directory)))

(defun %descriptor-alist-assoc-by-path (desc-alist val)
  (assoc val desc-alist
         :key #'pndescriptor-path
         :test #'equal))

(defun pndescriptor-alist-assoc (desc-alist lookup-type lookup-val)
  (let ((lookup-fn (case lookup-type
                     (:data #'%descriptor-alist-assoc-by-data)
                     (:uuid #'%descriptor-alist-assoc-by-uuid)
                     (:path #'%descriptor-alist-assoc-by-path)
                     (otherwise (error "Unknown DESCRIPTOR-ALIST lookup type!")))))
    (funcall lookup-fn desc-alist lookup-val)))

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

(defun rchain-pndescriptor-paths (pndescriptors &key file-expected-p relative-expected-p)
  "RCHAIN-PNDESCRIPTOR-PATHS

Chains the paths of each pndescriptor, in REVERSE order. EXAMPLE:

Merging a list of pndescriptors (CD BD AD) that would have the paths ('c' 'b/' '/a/'),
correspondingly, will generate '/a/b/c'.
"
  (let ((result (%rchain-pndescriptor-paths pndescriptors :file-expected-p file-expected-p)))
    (cond
      ((and relative-expected-p (absolute-pathname-p result))
       (error "Descriptor chaining resulted in absolute path, while relative path was expected!"))
      ((and (not relative-expected-p) (relative-pathname-p result))
       (error "Descriptor chaining resulted in relative path, while absolute path was expected!"))
      (t result))))
