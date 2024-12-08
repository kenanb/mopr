;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;

(in-package #:mopr-sgt)

(defstruct (dnode
            (:include bnode)
            (:copier nil)
            (:constructor))
  "Structure: DNODE

Represents the node content that's considered temporary, mainly resulting from
the preprocessing step before procedure execution.

Payload is stored directly on the node, to avoid the cost of calculating a
digest. Because payloads resulting from preprocessing step are often large, and
quantity of nodes is high when expanding animated procedures."

  (payload (error "A DNODE cannot be initialized without a payload.")
   :type payload))

(defmethod bnode-find-payload ((node dnode))
  (dnode-payload node))
