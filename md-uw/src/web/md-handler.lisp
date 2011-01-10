(in-package :md)

(defclass md-image-handler (bknr.web:object-handler)
  ())

(defmethod object-handler-get-object ((handler md-image-handler))
  (let ((id-or-name (bknr.web:parse-url))
        (params (mapcar #'url-decode (split "/" (script-name*)))))
    (when id-or-name
      (let ((object (find-store-object id-or-name)))
        (unless (member (type-of object)
                        (list 'md-pattern 'machine 'md-pattern-track 'kit))
          (error "wrong type of MD object ~A" object))
        object))))

(defun get-cached-image-file (file)
  (merge-pathnames file *image-cache-directory*))

(defun image-file-uptodate-p (file object)
  (and (file-exists-p file)
       (> (file-write-date file)
	  (md-object-edit-time object))
       (> (file-write-date file)
	  (md-object-creation-time object))))

(defun get-integer-parameter (name &optional (request *request*))
  (let ((string (get-parameter name request)))
    (when string
      (parse-integer string :junk-allowed t))))

(defmethod handle-object ((handler md-image-handler) object)
  (let* ((scale (min 4 (or (get-integer-parameter "scale") 1)))
         (name (format nil "~a-~A" (store-object-id object) scale))
         (type (symbol-name (md-object-type object)))
         (image-file (get-cached-image-file (make-pathname :directory (list :relative type)
                                                           :name name))))

    ;; (format t "cached file, object: ~A: ~A, scale ~A~%" object image-file scale)
    (if (image-file-uptodate-p image-file object)
        (handle-static-file image-file "image/png")
        (with-http-response (:content-type "image/png")
          (let* ((s (send-headers))
                 (img (scale-image (md-make-image object) scale)))
            (ensure-directories-exist image-file)
            (let ((white (allocate-color 255 255 255 :image img)))
              (setf (transparent-color img) white))
            (with-open-file (s2 image-file :direction :output :if-exists :overwrite :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
              (write-image-to-stream s2 :png :image img))
            (write-image-to-stream s :png :image img))))))
        
        