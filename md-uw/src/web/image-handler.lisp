(in-package :md)

(enable-interpol-syntax)

(defun scan-object-image-uri (uri)
  (multiple-value-bind (match strings)
      (scan-to-strings #?r"/image/([^/]+)/([^/,\?]+)\??([^/]*)" uri)
    (when match
      (list (aref strings 0)
	    (aref strings 1)))))

(defun scan-text-image-uri (uri)
  (multiple-value-bind (match strings)
      (scan-to-strings #?r"/text-image/([^/\?]+)\??.*" uri)
    (when match
      (aref strings 0))))

(defun get-cached-image-file (file)
  (merge-pathnames file *image-cache-dir*))

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

(defun text-image-page ()
  (setf (content-type) "image/png")
  (let ((text (url-decode (scan-text-image-uri (request-uri)))))
    (unless text
      (error "wrong url for text image: ~A" (request-uri)))

    (let* ((scale (min 4 (or (get-integer-parameter "scale") 1)))
	   (name (if (= scale 1) text
		     (format nil "~a-~a" text scale)))
	   (image-file (get-cached-image-file (make-pathname :directory (list :relative "text")
							     :name name))))
	  
      (if (file-exists-p image-file)
	  (handle-static-file image-file "image/png")
	  (let ((img (scale-image (md-text text) scale))
		(s (send-headers)))
	    (ensure-directories-exist image-file)
	    (let ((white (allocate-color 255 255 255 :image img)))
	      (setf (transparent-color img) white))
	    (write-image-to-file image-file :type :png :if-exists :overwrite :image img)
	    (write-image-to-stream s :png :image img))))))

(defun image-page ()
  (setf (content-type) "image/png")
  (let ((parse (scan-object-image-uri (request-uri))))
    (unless parse
      (error "wrong url for image: ~A" (request-uri)))

    (let* ((type (first parse))
	   (id-string (second parse))
	   (id (parse-integer id-string :junk-allowed t))
	   (object (store-object-with-id id))
	   (scale (min 4 (or (get-integer-parameter "scale") 1)))
	   (name (if (= scale 1) id-string
		     (format nil "~a-~a" id-string scale)))
	   (image-file (get-cached-image-file (make-pathname :directory (list :relative type)
							     :name name))))
      (unless object
	(error "no object with id for image: ~A" id-string))

      (unless (member (type-of object)
		      (list 'md-pattern 'machine 'md-pattern-track
			    'md-row 'md-song))
	(error "object of wrong type for image: ~A" object))

      (if (image-file-uptodate-p image-file object)
	  (handle-static-file image-file "image/png")
	  (let ((img (scale-image (md-make-image object) scale))
		(s (send-headers)))
	    (ensure-directories-exist image-file)
	    (let ((white (allocate-color 255 255 255 :image img)))
	      (setf (transparent-color img) white))
	    (write-image-to-file image-file :type :png :if-exists :overwrite :image img)
	    (write-image-to-stream s :png :image img))))))

