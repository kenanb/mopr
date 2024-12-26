;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-res)

(defstruct (descriptor
            (:constructor)
            (:constructor make-descriptor-for-file
                (file
                 &aux
                   (uuid (frugal-uuid:to-string (frugal-uuid:make-v7)))
                   (path (or (file-pathname-p file)
                             (error "File descriptor requested for non-file path!")))))
            (:constructor make-descriptor-for-directory
                (directory
                 &aux
                   (uuid (frugal-uuid:to-string (frugal-uuid:make-v7)))
                   (path (ensure-directory-pathname directory)))))
  "DESCRIPTOR

A descriptor represents the means to unambiguously refer to a resource or
resource grouping.

Every entity that has an associated descriptor will be associated with a UUIDv7
at creation-time, so that the underlying content can be moved while maintaining
stable addressing by the client application.
"
  (uuid (error "DESCRIPTOR cannot be initialized without a uuid!")
   :type (simple-base-string 36)
   :read-only t)
  (path (error "DESCRIPTOR cannot be initialized without a path!")
   :type pathname
   :read-only t))

(defun rchain-descriptor-uuids (descriptors)
  "RCHAIN-DESCRIPTOR-UUIDS

Chains the uuids of each descriptor, in REVERSE order. EXAMPLE:

Merging a list of descriptors (CD BD AD) that would have the uuids ('2' '1' '0'),
correspondingly, will generate '0/1/2'.
"
  (format nil "~{~A~^/~}" (reverse (mapcar #'descriptor-uuid descriptors))))

(defun %rchain-descriptor-paths (descriptors &key file-expected-p
                                 &aux
                                   (p (descriptor-path (car descriptors)))
                                   (r (cdr descriptors)))
  (cond
    ((and file-expected-p (directory-pathname-p p))
     (error "Directory path found while a file path was expected in descriptor!"))
    ((and (not file-expected-p) (file-pathname-p p))
     (error "File path found while a directory path was expected in descriptor!"))
    ((and r (absolute-pathname-p p))
     (error "Non-terminating descriptor path cannot be absolute!"))
    (r (merge-pathnames* p (%rchain-descriptor-paths r)))
    (t p)))

(defun rchain-descriptor-paths (descriptors &key file-expected-p relative-expected-p)
  "RCHAIN-DESCRIPTOR-PATHS

Chains the paths of each descriptor, in REVERSE order. EXAMPLE:

Merging a list of descriptors (CD BD AD) that would have the paths ('c' 'b/' '/a/'),
correspondingly, will generate '/a/b/c'.
"
  (let ((result (%rchain-descriptor-paths descriptors :file-expected-p file-expected-p)))
    (cond
      ((and relative-expected-p (absolute-pathname-p result))
       (error "Descriptor chaining resulted in absolute path, while relative path was expected!"))
      ((and (not relative-expected-p) (relative-pathname-p result))
       (error "Descriptor chaining resulted in relative path, while absolute path was expected!"))
      (t result))))

(defun descriptor-alist-assoc-by-data (desc-alist val)
  (rassoc val desc-alist))

(defun descriptor-alist-assoc-by-uuid (desc-alist val)
  (assoc val desc-alist
         :key #'descriptor-uuid
         :test #'equal))

(defun descriptor-alist-assoc-by-path (desc-alist val)
  (assoc val desc-alist
         :key #'descriptor-path
         :test #'equal))

(defun descriptor-alist-assoc (desc-alist lookup-type lookup-val)
  (let ((lookup-fn (case lookup-type
                     (:data #'descriptor-alist-assoc-by-data)
                     (:uuid #'descriptor-alist-assoc-by-uuid)
                     (:path #'descriptor-alist-assoc-by-path)
                     (otherwise (error "Unknown DESCRIPTOR-ALIST lookup type!")))))
    (funcall lookup-fn desc-alist lookup-val)))
