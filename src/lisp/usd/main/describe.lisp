(in-package #:mopr-usd)

(defmethod describe-object :after ((stage-h mopr-stage-h) stream)
  (with-handles* ((layer-h :layer)
                  (string-h :string))
    (stage-get-root-layer-w layer-h stage-h)
    (when (layer-try-upgrade layer-h)
      (layer-get-identifier string-h layer-h)
      ;; Ptr points to the internal data of std::string, so no need to free separately.
      ;; (multiple-value-bind (id ptr) (string-cstr string-h) (autowrap:free ptr) ...)
      (format stream "~%ROOT IDENTIFIER: ~S~%" (string-cstr string-h)))))

(defmethod describe-object ((o mopr-layer-h) stream) (format stream "LAYER-H: ~S~%" o))

(defmethod describe-object ((o mopr-path-h) stream) (format stream "PATH-H: ~S~%" o))

(defmethod describe-object ((o mopr-token-h) stream) (format stream "TOKEN-H: ~S~%" o))

(defmethod describe-object ((o mopr-string-h) stream) (format stream "STRING-H: ~S~%" o))
