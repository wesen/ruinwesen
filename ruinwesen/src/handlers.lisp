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




(defclass parenscript-file-handler (prefix-handler)
  ((destination :initarg :destination
		:reader parenscript-handler-destination)))

(defmethod handle :around ((handler parenscript-file-handler))
  (let* ((script-name (subseq (script-name*)
			     (length (page-handler-prefix handler))))
	 (script-file (merge-pathnames script-name
				       (parenscript-handler-destination handler))))

    (with-http-response (:content-type "application/javascript; charset=UTF-8")
      (no-cache)
      (with-output-to-string (sout)
	(with-open-file (s script-file :direction :input)
	  (loop for elt = (read s  nil :eof)
	     until (eq elt :eof)
	     do (princ
		 (parenscript:compile-script elt) sout)
	       (princ #\Newline sout)))))))

(defclass javascript-handler ()
  ())

(defvar *js-stream*)

(defmethod handle :around ((handler javascript-handler))
  (with-http-response (:content-type "text/html; charset=UTF-8")
    (no-cache)
    (with-http-body ()
      (html
       ((:script :language "JavaScript")
        (:princ
         (with-output-to-string (*js-stream*)
           (call-next-method))))))))

(defclass login-js-handler (javascript-handler page-handler)
  ())

(defmethod handle ((handler login-js-handler))
  (format *js-stream*
	  (parenscript:ps
	    (parent.login-complete (ps:lisp (if (admin-p (bknr-session-user)) t 'false))
				   (ps:lisp (user-login (bknr-session-user)))))))

;; upload handlers

(defclass soundfile-handler (blob-handler)
  ())

(defclass edit-news-handler (admin-only-handler page-handler)
  ())

(defmethod handle ((handler edit-news-handler))
  (with-query-params (action title text id)
    (when id
      (setf id (parse-integer id :junk-allowed t)))
    (cond ((null id)
	   (make-instance 'ruinwesen-news
			:title title
			:owners (list (bknr-session-user))
			:text text
			:date (get-universal-time))
	   (redirect "/news"))
	  (t
	   (let ((news (store-object-with-id id)))
	     (if (string= action "delete")
		 (progn
		   (delete-object news)
		   (redirect "/news"))
		 (progn (with-transaction ()
			  (setf (ruinwesen-news-title news) title
				(ruinwesen-news-text news) text))
			(redirect (format nil "/news/~A" id)))))))))

(defclass edit-product-handler (admin-only-handler page-handler)
  ())

(deftransaction add-picture-to-product (product image)
  (push image (product-pictures product)))

(defmethod edit-product-media ((product product) picture media mediadescription)
  (when picture
    (with-image-from-upload* (picture)
      (setf picture (make-store-image :name
				      (format nil "~A-~A"
					      (pathname-name (upload-original-filename
							      picture))
					      (get-universal-time))))))
  (when media
    (setf media (make-blob-from-file (upload-pathname media) 'soundfile
				     :description mediadescription
				     :mime-type "audio/mpeg")))
  (with-transaction ()
    (when picture
      (add-picture-to-product product picture))
    (when media
      (push media (product-soundfiles product))))
  product)

(defun keywords-from-string (keywords)
  (when keywords 
    (mapcar #'make-keyword-from-string
	    (split "\\s*,\\s*" keywords))))

(defmethod handle ((handler edit-product-handler))
  (with-query-params (action name description keywords price id pictureid
			     mediaid mediadescription)
    (when id
      (setf id (parse-integer id :junk-allowed t)))
    (when pictureid
      (setf pictureid (parse-integer pictureid)))
    (setf keywords (keywords-from-string keywords))
    (when mediaid
      (setf mediaid (parse-integer mediaid)))
    (cond ((and (null id) (string= action "edit"))
	   (let ((product nil)
		 (picture
		    (request-uploaded-file "image-file"))
		 (media (request-uploaded-file "sound-file")))

	     (edit-product-media (make-instance 'product :name name
					:owners (list (bknr-session-user))
					:description description
					:keywords keywords
					:price (make-money (floor (parse-number:parse-real-number price)) :USD))
			  picture media mediadescription)
	     (redirect (format nil "/products/~A" (store-object-id product)))))

	  ((and id (string= action "deletepicture") pictureid)
	   (let ((product (store-object-with-id id))
		 (picture (store-object-with-id pictureid)))
		   
	   (with-transaction ()
	     (setf (product-pictures product)
		   (remove picture (product-pictures product)))
	     (delete-object picture))
	   (redirect (format nil "/products/~A" id))))

	  ((and id (string= action "deletemedia") mediaid)
	   (let ((product (store-object-with-id id))
		 (media (store-object-with-id mediaid)))
	   (with-transaction ()
	     (setf (product-soundfiles product)
		   (remove media (product-soundfiles product)))
	     (delete-object media))
	   (redirect (format nil "/products/~A" id))))

	  ((and id (string= action "editmedia") mediaid)
	   (let ((media (store-object-with-id mediaid)))
	     (with-transaction ()
	       (setf (soundfile-description media) mediadescription))
	     (redirect (format nil "/products/~A" id))))

	  ((and id (string= action "edit"))
	   (let ((product (store-object-with-id id))
		 (picture (request-uploaded-file "image-file"))
		 (media (request-uploaded-file "sound-file")))

	     (edit-product-media product picture media mediadescription)
	     (with-transaction ()
	       (setf (product-description product) description
		     (product-keywords product) keywords
		     (product-price product) (make-money (parse-integer price) :USD))))
	   (redirect (format nil "/products/~A" id)))
	  
	  ((and id (string= action "delete"))
	   (delete-object (store-object-with-id id))
	   (redirect "/products"))
	  
	  ((and id (string= action "newpicture"))
	   (let* ((uploaded-file (request-uploaded-file "image-file"))
		  (product (store-object-with-id id)))
	     (with-transaction ()
	       (with-image-from-upload* (uploaded-file)
		 (let* ((image (make-store-image :name (pathname-name
							(upload-original-filename uploaded-file)))))
		   (push image (product-pictures product))))))
	   (redirect (format nil "/products/~A" id)))
	  
	  
	  ((and id (string= action "newmedia"))
	   (let* ((uploaded-file (request-uploaded-file "sound-file"))
		  (product (store-object-with-id id)))
	     (with-transaction ()
	       (let* ((soundfile (make-blob-from-file uploaded-file)))
		 (push soundfile (product-soundfiles product)))))
	   (redirect (format nil "/products/~A" id))))))

    

(defclass upload-image-handler (admin-only-handler page-handler)
  ())

(defmethod handle ((handler upload-image-handler))
  (with-query-params ()
    (let ((uploaded-file (request-uploaded-file "image-file")))
      (handler-case
	  (progn
	    (unless uploaded-file
	      (error "no file uploaded"))
	    (with-image-from-upload* (uploaded-file)
	      (let* ((image (make-store-image :name (pathname-name (upload-original-filename uploaded-file)))))
		(with-http-response ()
		  (with-http-body ()
		    (html (:html
			   (:head
			    (:title "Upload successful")
			    ((:script :type "text/javascript" :language "JavaScript")
			     "function done() { window.close(); }"))
			   (:body
			    (:p "Image " (:princ-safe (store-image-name image)))
			    (:p ((:img :src (format nil "/image/~D" (store-object-id image))))
			    (:p ((:a :href "javascript:done()") "ok")))))))))))
	(error (e)
	  (with-http-response ()
	    (with-http-body ()
	      (html (:html
		     (:head
		      (:title "Error during upload"))
		     (:body
		      (:h2 "Error during upload")
		      (:p "Error during upload:")
		      (:p (:princ-safe (apply #'format nil (simple-condition-format-control e) (simple-condition-format-arguments e))))
		      (:p ((:a :href "javascript:window.close()") "ok"))))))))))))

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
					

#|
(defclass edit-image-js-handler (admin-only-handler javascript-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'quickhoney-image))

(defmethod handle-object-form ((handler edit-image-js-handler) action image)
  (format t "; invalid action ~A or invalid object ~A~%" action image))

(defmethod handle-object-form ((handler edit-image-js-handler) (action (eql :edit)) image)
  (with-query-params (client spider-keywords)
    (with-transaction (:edit-image)
      (setf (quickhoney-image-client image) client
            (quickhoney-image-spider-keywords image) spider-keywords))
    (format *js-stream* "parent.image_edited()~%")))

(defmethod handle-object-form ((handler edit-image-js-handler) (action (eql :delete)) (image quickhoney-image))
  (delete-object image)
  (format *js-stream* "parent.image_deleted();~%"))


|#