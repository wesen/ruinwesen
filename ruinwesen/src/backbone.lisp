(in-package :ruinwesen)

(defclass backbone-handler (object-handler)
  ())

(defmethod handle ((handler backbone-handler))
  (let ((id (parse-url)))
    (with-output-to-string (stream)
      (case (hunchentoot:request-method*)
        (:GET (if id
                  (yason:encode (object-handler-get-object handler) stream)
                  (yason:encode (store-objects-with-class (object-handler-object-class handler)))))
        (:POST
         (let ((msg (yason:parse (hunchentoot:raw-post-data :force-text t))))
           (yason:encode msg stream)))
        (:PUT
         (yason:encode (alexandria:plist-hash-table '()) stream))
        (:DELETE "")))))