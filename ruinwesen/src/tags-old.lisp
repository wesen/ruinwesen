(in-package :ruinwesen.tags)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\$ #'(lambda (stream char char2)
					    (list '$ (read stream t nil t)))))

(define-bknr-tag header ()
  (dolist (css '("default.css" "alphacube.css" "styles.css"))
    (html ((:link :rel "stylesheet" :href (format nil "/static/css/~A" css)))))
  (dolist (js '("prototype.js" "window.js" "window_ext.js" "effects.js" "debug.js"))
    (html ((:script :type "text/javascript" :src (format nil "/static/js/~A" js)) " ")))
  (html ((:script :type "text/javascript" :src "/js/ruinwesen") " ")))

(define-bknr-tag navigation (&key where)
  (html
   ((:div :id "name")
    (:h1 "ruin & wesen electronics"))
   ((:p :id "info")
    (dolist (elt '(("home" . ("/" "/index"))
		   ("about" . "/about")
		   ("news" . "/news")
		   ("products" . "/products")
		   ("gallery" . "/gallery")
		   ("contact" . "/contact")))
      (if (if (listp (cdr elt))
	      (member (hunchentoot:script-name*) (cdr elt) :test #'string=)
	      (string= (hunchentoot:script-name*) (cdr elt)))
	    (html (:span (:princ-safe (car elt))))
	    (html ((:a :href (if (listp (cdr elt))
				 (first (cdr elt))
				 (cdr elt)))
		   (:princ-safe (car elt)))))))))

(defun first-elts (list num)
  (if (< (length list) num)
      list
      (subseq list 0 num)))

(define-bknr-tag news-edit (news)
  (html ((:div :id "edit_news_form" :class "cms_form")
	 ((:div :class "cms_title") "Edit News")
	 ((:form :action "/edit-news" :method "post")
	  ((:p :class "cms")
	   (:table (:tbody
		    (:tr (:td "Title:")
			 (:td ((:input :type "text" :size 50 :name "title"
				       :value (if news (ruinwesen-news-title news) "")))))
		    (:tr (:td "Text:")
			 (:td ((:textarea :cols 50 :rows 10 :name "text")
			       (:princ-safe (if news (ruinwesen-news-text news) "")))))
		    ((:input :type "hidden" :name "id" :value (when news (store-object-id news)))))))
	  ((:p :class "cms")
	   ((:input :type "submit" :name "action" :value "edit"))
	   ((:input :type "submit" :name "action" :value "delete")))))))

(define-bknr-tag news ()
  (flet ((news-box (news)
	  (html ((:div :class "news")
		 ((:div :class "header")
		  ((:span :class "date") (:princ (bknr.utils:format-date-time
						 (ruinwesen-news-date news))))
		  ((:span :class "title")
		   ((:a :href (format nil "/news/~A" (store-object-id news)))
		    (:princ (ruinwesen-news-title news))))
		  (:p " "))
		 ((:p :class "text") 
		  (:princ (ruinwesen-news-text news)))))))
    
    (let* ((url (parse-url))
	   (id (when url (parse-integer (parse-url) :junk-allowed t))))
      (if id
	  (let ((news (store-object-with-id id)))
	    (news-box news)
	    (if (admin-p (bknr-session-user))
		(news-edit news)))
	  (let ((newses (first-elts (sort (copy-tree (store-objects-with-class 'ruinwesen-news)) #'>
					  :key #'ruinwesen-news-date) 10)))
	    (dolist (news newses)
	      (news-box news))
	    (if (admin-p (bknr-session-user))
		(news-edit nil)))))))

(defun print-price (price &key in-eur)
  (if in-eur
      (format nil "~A EUR" (round-to (/ (amount price) ruinwesen.config::*dollars-to-eur*)
				    10))
      (format nil "~A USD" (amount price))))

(define-bknr-tag product-page (&key name)
  (let* ((product (product-with-name name))
	 (imgs (sort (copy-tree (product-pictures product)) #'< :key #'store-object-id)))
    (unless product
      (error "no such product"))
    (html ((:div :id "product")
	   (:h1 (:princ-safe (product-name product)))
	   ((:div :id "imgbrws")
	    (unless (null imgs)
	      (html
	       ((:a :id "imgbrwsback" :href "#" :onclick (ps:ps-inline (img-go-back))) "<<<")
	       ((:img :id "productimg"))
	       ((:a :id "imgbrwsnext" :href "#" :onclick (ps:ps-inline (img-go-next))) ">>>")))
	    ((:script :language "JavaScript")
	     (:princ (format nil "var imgArray = [~{\"/image/~A/thumbnail,,300,300\"~#[~:;, ~]~}];" (mapcar #'store-object-id imgs)))
	     (:princ (ps:ps (defvar img-idx 0)
			    (defun set-image (idx)
			      (if (== idx 0)
				(hide #$"imgbrwsback")
				(show #$"imgbrwsback"))
			      (if (== idx (1- img-array.length))
				(hide #$"imgbrwsnext")
				(show #$"imgbrwsnext"))
			      (setf (slot-value #$"productimg" 'src) (aref img-array idx)))
			    (defun img-go-back ()
			      (unless (== img-idx 0)
				(decf img-idx))
			      (set-image img-idx))
			    (defun img-go-next ()
			      (unless (== img-idx (1- img-array.length))
				(incf img-idx))
			      (set-image img-idx))
			    (set-image 0)))))
	    
	   ((:p :class "description") (:princ-safe (product-description product)))
	   ((:p :class "soundfiles")
	    (loop for soundfile in (sort (copy-tree (product-soundfiles product)) #'< :key #'store-object-id)
		 for i from 1
		 do
		 (html ((:div :class "soundfile")
			( (:p :class "sounddescription") "Soundfile " (:princ i) ": "
			  (:princ-safe (soundfile-description soundfile)))
			(soundfile :id (store-object-id soundfile))))))
	   ((:div :class "order")
	    (:ul
	     (:li
	      ((:div :class "white")
	       (:Princ-safe (print-price (product-price product)))
	       :br
	       (product-paypal :name name)
	       " "))
	     (:li
	      ((:div :class "white")
	       (:Princ-safe (print-price (product-price product) :in-eur t))
	       :br
	       (product-paypal :name name :in-eur t) " ")))))
	  (if (admin-p (bknr-session-user))
	(product-edit product)))))
	   
(defun group-by (list num)
  (loop for group on list by #'(lambda (seq) (subseq seq (min (length seq) num)))
	collect (subseq group 0 (min (length group) num))))


(defun random-elt (list)
  (elt list (random (length list))))

(define-bknr-tag products ()
  (let* ((url (parse-url))
	 (id (when url (parse-integer (parse-url) :junk-allowed t))))
    (if id
	(product-page :name (product-name (store-object-with-id id)))
	(html ((:div :id "products")  ""
	       ((:div :class "middle")
		(:table " "
		(dolist (group (group-by (remove-if #'(lambda (product)
							(member :gallery (product-keywords product)))
						    (store-objects-with-class 'product)) 3))
		  (html (:tr
			 (dolist (product group)
			  (let ((picture-ids (or (mapcar #'store-object-id
							 (product-pictures product))
						 (list "productlogo"))))
			    (html (:td ((:div :class "product_browse")
					((:a :href (format nil "/products/~A" (store-object-id product)))
					 ((:img :src (format nil "/image/~A/thumbnail,,170,170"
							     (random-elt picture-ids)) :border 0)))
					:br
					(:princ-safe (product-name product))))))))))
		)))
	       (if (admin-p (bknr-session-user))
		   (product-edit nil))))))

(define-bknr-tag footer ()
  (html #+nil((:div :id "footerspacer") "blabla " )
	((:div :id "footer")
	 ((:div :align "center")
	  #+nil(:h3 "handmade analog and digital musical instruments")))))
  
(define-bknr-tag gallery ()
  (let* ((url (parse-url))
	 (id (when url (parse-integer (parse-url) :junk-allowed t))))
    (if id
	(product-page :name (product-name (store-object-with-id id)))
	(html ((:div :id "products")  ""
	       ((:div :class "middle")
	       (:table " "
		(dolist (group (group-by (ruinwesen::get-keyword-products :gallery) 4))
		  (html (:tr
			 (dolist (product group)
			  (let ((picture-ids (or (mapcar #'store-object-id
							 (product-pictures product))
						 (list "productlogo"))))
			    (html ((:td)
				       ((:div :class "product_browse")
					((:a :href (format nil "/products/~A" (store-object-id product)))
					 ((:img :src (format nil "/image/~A/thumbnail,,120,120/gray"
							     (random-elt picture-ids)) :border 0)))
					:br
					(:princ-safe (product-name product))))))))))
		)))
	       (if (admin-p (bknr-session-user))
		   (product-edit nil))))))


(define-bknr-tag soundfile (&key id)
  (let ((soundfile (store-object-with-id id)))
    (html ((:script :language "JavaScript"
		    :src "/static/js/audio-player.js") " ")
	  ((:object :type "application/x-shockwave-flash"
		    :data "/static/player.swf"
		    :id (format nil "audioplayer~A" id) :height 24 :width 290)
	   ((:param :name "movie" :value "/static/player.swf"))
	   ((:param :name "FlashVars" :value
		    (format nil "playerID=~A&soundFile=~Amedia/~A"
			    id *website-url* id)))
	   ((:param :name "quality" :value "high"))
	   ((:param :name "menu" :value "false"))
	   ((:param :name "wmode" :value "transparent"))))))

(defun keywords-to-string (keywords)
  (format nil "~{~(~A~)~#[~:;,~]~}" keywords))
  
(define-bknr-tag product-edit (product)
  (html ((:div :id "edit_product_form" :class "cms_form")
	 ((:div :class "cms_title") "Edit Product")
	 ((:form :enctype "multipart/form-data" :action "/edit-product" :method "post")
	  ((:p :class "cms")
	   (:table (:tbody
		    (:tr (:td "Name:")
			 (:td
			  (if product
			      (html (:princ-safe (product-name product)))
			      (html ((:input :type "text" :size 40 :name "name"
					     :value (if product (product-name product) "")))))))
		    (:tr (:td "Price:")
			 (:td ((:input :type "text" :size 40 :name "price"
				       :value (if (and product (product-price product))
						  (amount (product-price product)) "")))))
		    (:tr (:td "Keywords:")
			 (:td ((:input :type "text" :size 40 :name "keywords"
				       :value (if product (keywords-to-string
							   (product-keywords product)) "")))))
		    (:tr (:td "Description:")
			 (:td ((:textarea :cols 40 :rows 15 :name "description")
			       (:princ-safe (if product (product-description product) "")))))
		    ((:input :type "hidden" :name "id" :value (when product (store-object-id product))))
		    (:tr (:td "Images:")
			 (:td ((:input :type "file" :name "image-file"))))
		    
		    (:tr (:td "Soundfiles:")
			 (:td((:input :type "file" :name "sound-file"))
			      ((:textarea :cols 40 :rows 4 :name "mediadescription") "")
			      )))))
	   
	   ((:p :class "cms")
	    ((:input :type "submit" :name "action" :value "edit"))
	    ((:input :type "submit" :name "action" :value "delete"))))

	 (when product
	     (dolist (media (product-soundfiles product))
	       (html ((:p :class "cms")
		      ((:form :action "/edit-product" :method "post")
		       (:table (:tbody
				(:tr (:td "File:")
				     (:td (:princ-safe (blob-pathname media))))
				(:tr (:td "Description: ")
				     (:td ((:textarea :cols 30 :rows 4
						      :name "mediadescription")
					   (:princ (soundfile-description media)))))
				((:input :type "hidden" :name "id" :value (store-object-id product)))
				((:input :type "hidden" :name "mediaid" :value (store-object-id media)))))
		       ((:p :class "cms")
			((:input :type "submit" :name "action" :value "editmedia"))
			((:input :type "submit" :name "action" :value "deletemedia")))))))
	     (dolist (picture (product-pictures product))
	       (html ((:p :class "cms")
		      ((:form :action "/edit-product" :method "post")
		       (:table (:tbody
				(:tr (:td "Picture:")
				     (:td ((:img :src (format nil "/image/~A/thumbnail,,50,50"
							      (store-object-id picture)))))))
				((:input :type "hidden" :name "id" :value (store-object-id product)))
				((:input :type "hidden" :name "pictureid"
					 :value (store-object-id picture))))
		       ((:p :class "cms")
			((:input :type "submit" :name "action" :value "deletepicture")))))))))))
  

(defun list-to-hidden-inputs (list)
  (loop for (var . val) in list
       do (html ((:input :type "hidden" :name var :value val)))))

(defparameter *paypal-cancel-url*
  (puri:render-uri (puri:merge-uris "/cancelpaypal" *website-url*) nil))

(defparameter *paypal-return-url*
  (puri:render-uri (puri:merge-uris "/returnpaypal" *website-url*) nil))

(defun round-to (num to)
  (* to (round num to)))

(define-bknr-tag product-paypal (&key name in-eur)
  (let* ((product (ruinwesen:product-with-name name))
	 (name (ruinwesen:product-name product))
	 (price (ruinwesen:product-price product))
	 (currency (ruinwesen:currency price))
	 (amount (ruinwesen:amount price)))
    (when in-eur
      (setf amount (round-to (/ amount ruinwesen.config::*dollars-to-eur*) 10)
	    currency :EUR))
    (html ((:form :action "https://www.sandbox.paypal.com/us/cgi-bin/webscr"
		  :method "post")

	   (list-to-hidden-inputs `(("cmd" . "_xclick")
				    ("business" . "foobar_1206833805_biz@bl0rg.net")
				    ("currency_code" . ,currency)
				    ("item_name" . ,name)
				    ("amount" . ,amount)
				    ("cancel_return" . ,*paypal-cancel-url*)
				    ("return" . ,*paypal-return-url*)))
	   ((:input :type "image"
		    :src "http://www.sandbox.paypal.com/en_US/i/btn/x-click-but01.gif"
		    :name "submit"
		    :alt "Paypal"))))))

(define-bknr-tag contact ()
  (with-query-params ((subject "")  (name "") (email "") (text ""))
    (let ((errors ruinwesen::*contact-error*))
      (html
       ((:div :id "contact")
	(if errors
	    (dolist (error errors)
	      (html ((:p :style "color:red;font-weight:bold") (:princ-safe error))))
	    (html 
	     (:p "Please contact us!")))
	((:form :action "/submit-contact" :method "post")
	 (:table
	  (:tbody
	   (:tr (:td "Subject:")
		(:td ((:input :type "text" :size "35" :name "subject" :value subject))))
	   (:tr (:td "Your Name:")
		(:td ((:input :type "text" :size "35" :name "name" :value name))))
	   (:tr (:td "Your Email (required):")
		(:td ((:input :type "text" :size "35" :name "email" :value email))))
	   (:tr ((:td :colspan "2")
		 ((:textarea :cols "60" :rows "10" :name "text") (:princ-safe text))))
	   (:tr ((:td :colspan "2")
		 ((:input :type "submit" :name "action" :value "send!"))))))))))))
  

(define-bknr-tag include-file (&key name)
  (let ((file (merge-pathnames name *website-directory*)))
    (html
     (:princ
      (with-open-file (s file :direction :input)
	(read-file s))))))