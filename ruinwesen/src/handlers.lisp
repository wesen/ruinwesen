(in-package :ruinwesen)

(bknr.images::define-imageproc-handler bknr.images::gray (input-image)
  (let* ((width (image-width input-image))
	 (height (image-height input-image))
	 (gray-image (create-image width height nil)))
    (with-default-image (gray-image)
      (fill-image 0 0 :color (allocate-color 255 255 255))
      (copy-image
       ; input-image gray-image
       gray-image input-image
       0 0 0 0 width height
		  :merge-gray 0))
    input-image))

;; upload handlers
(defclass contact-handler (template-handler)
  ())

(defmethod handler-matches-p ((handler contact-handler))
  (string= (script-name*) "/submit-contact"))

(defvar *contact-error* nil)

(defmethod handle ((handler contact-handler))
  (with-query-params (name email city text)
    (let ((error nil)
	  (*contact-error* nil))
      (when (null email)
	(setf error t)
	(push "Please provide an email!" *contact-error*))
      (when (null text)
	(setf error t)
	(push "Please provide a message!" *contact-error*))
      (if error
	  (expand-template handler "contact")
	  (progn
	    (make-instance 'rw-contact :name name :email email :city city :text text)
	    (cl-smtp:send-email "localhost" email "info@ruinwesen.com"
				(format nil "Ruinwesen: Message from ~A of ~A" name city)
				(format nil
					"Message from ~A (~A) sent over ruinwesen.com:~%~%~A~%"
					name email text))
	    (expand-template handler "contact-thx"))))))
					

(defclass preorder-handler (template-handler)
  ())

(defmethod handler-matches-p ((handler preorder-handler))
  (string= (script-name*) "/submit-preorder"))

(defvar *preorder-error* nil)

(defmethod handle ((handler preorder-handler))
  (with-query-params (name email country form-factor device text)
    (let ((error nil)
	  (*preorder-error* nil))
      (when (null email)
	(setf error t)
	(push "Please provide an email!" *preorder-error*))
      (when (null device)
	(setf error t)
	(push "Please provide an device!" *preorder-error*))
      (when (null form-factor)
	(setf error t)
	(push "Please provide a form factor!" *preorder-error*))
      (if error
	  (expand-template handler "preorder")
	  (progn
	    (make-instance 'preorder :name name :email email :country country :form-factor form-factor
			 :device device :text text)
	    (cl-smtp:send-email "localhost" email "info@ruinwesen.com"
				(format nil "Ruinwesen: Preorder from ~A of ~A" name country)
				(format nil
					"Preorder from ~A (~A) sent over ruinwesen.com:
~%~%
Device: ~A~%
Form-Factor: ~A~%
Country: ~A~%
~A~%"
					name email device form-factor country text))
	    (expand-template handler "preorder-thx"))))))
