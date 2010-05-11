(in-package :ruinwesen.tags)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\$ #'(lambda (stream char char2)
					    (list '$ (read stream t nil t)))))

(define-bknr-tag embedmp3 (&key href)
  (html ((:object :type "application/x-shockwave-flash" :height 20 :width 100 :style "height: 15px; position: relative; top: 5px;")
	 ((:param :name "movie" :value "/static/media/player.swf"))
	 ((:param :name "quality" :value "high"))
	 ((:param :name "menu" :value "false"))
	 ((:param :name "wmode" :value "transparent"))
	 ((:param :name "FlashVars" :value (format nil "soundFile=~A" href))))))

(define-bknr-tag navigation (&key where)
  (html ((:div :class "rightcontainer")
	 ((:div :id "navigation")
	  (:ul
	   (dolist (elt '(("HOME" . "/index")
			  ("DIGITAL" . "/digital")
			  ("ANALOG" .  "/analog")
			  ("SUPPORT" . "/support")
			  ("BLOG" . "/blog")
			  ("ABOUT" . "/about")
			  ("CONTACT" . "/contact")))
	     (cond
	       ((if (listp (cdr elt))
		    (member (hunchentoot:script-name*) (cdr elt) :test #'string=)
		    (string= (hunchentoot:script-name*) (cdr elt)))
		(html ((:li :class "active") ((:a :href (if (listp (cdr elt))
							    (first (cdr elt))
							    (cdr elt)))
					      (:princ-safe (car elt))))))
	       ((string= (car elt) "CONTACT")
		(html ((:li :class "last") ((:a :href (if (listp (cdr elt))
							    (first (cdr elt))
							    (cdr elt)))
					      (:princ-safe (car elt))))))
	       (t
		 (html (:li ((:a :href (if (listp (cdr elt))
					   (first (cdr elt))
					   (cdr elt)))
			     (:princ-safe (car elt)))))))))))))
	      
(define-bknr-tag footer (&key (link nil))
  (html #+nil((:div :class "spacer") (:princ "&nbsp; &nbsp; "))

	((:div :class "footer")
	 ((:div :class "alignleft")
	  "2008 "
	  (if link (html ((:a :href "http://ruinwesen.com/") "Ruin & Wesen"))
	      (html (:princ-safe "Ruin & Wesen")))
	  " / Powered by " ((:a :href "http://bknr.net/" :title "bknr") "bknr") " / Design by "
	  ((:a :href "http://www.actueldesign.com" :title "Actuel Design") "Actuel Design"))
	 (:br)
	 ((:div :class "footright")
	  ((:a :href "http://ruinwesen.com/rss/ruinwesen-blog" :title "RSS")
	   ((:img :src "/static/images/rss.png" :alt "RSS")))
	  ((:a :href "http://validator.w3.org/check?uri=http%3A%2F%2Fruinwesen.com&charset=(detect+automatically)&doctype=Inline&group=0" :title "XHTML Validation")
	   ((:img :src "/static/images/xhtml.png" :alt "XHTML LINK")))
	  ((:a :href "http://jigsaw.w3.org/css-validator/validator?uri=http%3A%2F%2Fruinwesen.com&profile=css21&usermedium=all&warning=1&lang=en" :title "CSS Validation")
	   ((:img :src "/static/images/css.png" :alt "CSS")))))))


(define-bknr-tag html-header (&key (title "Ruin &amp; Wesen"))
  (with-query-params ((id nil))
    (unless (null id)
      (let ((post (store-object-with-id (parse-integer id))))
	(when (typep post 'ruinwesen::rw-blog-article)
	  (setf title (format nil "Ruin &amp; Wesen: ~A" (bknr.text:article-subject post)))))))
  
  (html (:head
	 ((:meta :http-equiv "Content-Type"
		 :content "text/html; charset=UTF-8"))
	 (:title (:princ title))
	 (:princ "<!--[if IE]> <link href=\"/static/css/ie_fix.css\" rel=\"stylesheet\" type=\"text/css\"> <![endif]-->")
	 ((:link :media "screen and (min-device-width: 481px)" :rel "stylesheet" :type "text/css" :href "/static/css/style.css"))
	 ((:link :media "only screen and (max-device-width: 480px)" :rel "stylesheet" :type "text/css" :href "/static/css/style-small.css"))

	 (unless (is-iphone-p)
	   (html
	    ((:link :rel "stylesheet" :type "text/css" :href "/static/css/lightbox.css"))
	    ((:script :type "text/javascript" :src "/static/js/prototype.js") " ")
	    ((:script :type "text/javascript" :src "/static/js/scriptaculous.js?load=effects,builder") " ")
	    
	    ((:script :type "text/javascript" :src "/static/js/prototype.js") " ")))
	 (when (is-iphone-p)
	   (html
	    ((:script :type "text/javascript" :src "/static/js/iphone.js") " ")
	    ((:link :rel "apple-touch-icon" :href="/static/media/rw-square-iphone.png"))))
	   
	 ((:link :rel "alternate" :type "application/rss+xml"
		 :title "Ruin & Wesen Blog" :href "http://ruinwesen.com/rss/ruinwesen-blog"))
	 )))

(define-bknr-tag header ()
  (html ((:div :class "head")
	 ((:a :href "http://ruinwesen.com/" :title "Ruin & Wesen Home")
	  ((:img :src "/static/images/logo2.png" :id "logo" :alt "Logo")))
	 ((:div :class "social")
	  ((:div :class "twitter")
	   #-nil(blog-headlines)
	   #-nil(html (:br))
	   (twitter-headlines)
	   #-nil(html (:br))
	   #-nil(ruin-twitter-headlines)
	   (html (:br))
	   #+nil(blog-headlines)
	   (html (:br))
	   )))))

(define-bknr-tag blog-headlines ()
  (html ((:div :class "t_title")
	 ((:a :href "/blog" :title "Wesen Blog")
	  #+nil((:img :src "/static/images/bloggery.png" :alt "Blog Icon"))
	  "Ruin & Wesen Blog")
	 ((:div :class "sidebartext")
	  (dolist (post (ruinwesen::rw-posts))
	    (html ((:div :class "twit") (:princ-safe (bknr.text:article-subject post))
		   " " ((:a :href (format nil "/blog#post-~A" (store-object-id post)))
			(:princ-safe (ruinwesen::twitter-difference
				      (bknr.text:article-time post)))))
		  (:br)))))))

(define-bknr-tag news-headlines ()
  (html ((:div :class "t_title")
	 ((:a :href "/index" :title "Wesen News")
	  #+nil((:img :src "/static/images/bloggery.png" :alt "Blog Icon"))
	  "Ruin & Wesen News")
	 ((:div :class "sidebartext")
	  (dolist (post (ruinwesen::news-posts  :count 3))
	    (html ((:div :class "twit") (:princ-safe (bknr.text:article-subject post))
		   " " ((:a :href (format nil "/index#post-~A" (store-object-id post)))
			(:princ-safe (ruinwesen::twitter-difference
				      (bknr.text:article-time post)))))
		  (:br)))))))

(defun is-iphone-p ()
  (cl-ppcre:scan "Mobile.*Safari" (hunchentoot:user-agent)))

(define-bknr-tag banner ()

  (if (is-iphone-p)
      (html ((:div :class "banner") "  "))
      (html ((:div :class "banner" :align "center")
	     ((:img :src "/images/banner2.png" :alt "Banner"))))))

(define-bknr-tag twitter-headlines ()
  (html ((:div :class "t_title")
	 ((:a :href "http://twitter.com/wesen" :title "Wesen Twitter")
	  #+nil((:img :src "/static/images/twet.png" :alt "Twitter Icon"))
	  "What is Wesen doing?")
	 ((:div :class "sidebartext")
	  (if (null ruinwesen::*last-twitters*)
	      (html ((:div :class "twit") "No twitter status"))
	      (dolist (tweet ruinwesen::*last-twitters*)
		(html ((:div :class "twit") (:princ (ruinwesen::text-to-html2 (second tweet))) " "
		       ((:a :href (format nil "http://twitter.com/wesen/statuses/~A" (third tweet)))
			(:princ-safe (first tweet)))) (:br))))))))

(define-bknr-tag ruin-twitter-headlines ()
  (html ((:div :class "t_title")
	 ((:a :href "http://twitter.com/RuinMusic" :title "Ruin Twitter") "What is Ruin doing?")
	 ((:div :class "sidebartext")
	  (if (null ruinwesen::*last-twitters-ruin*)
	      (html ((:div :class "twit") "No twitter status"))
	      (dolist (tweet ruinwesen::*last-twitters-ruin*)
		(html ((:div :class "twit") (:princ-safe (second tweet)) " " ((:a :href "") (:princ-safe (first tweet)))) (:br))))))))

(defun print-post (post)
  (html (:h5 (:princ-safe (bknr.text:article-subject post))
	     ((:span :class "blogdate") (:princ-safe (format-date-time (bknr.text:article-time post)))))
	(when (and (typep post 'ruinwesen::rw-blog-article)
		   (not (null (ruinwesen::rw-blog-article-short post))))
	  (html (:p (:princ (ruinwesen::text-to-html2 (ruinwesen::rw-blog-article-short post))))))
	(when (bknr.text:article-text post)
	  (html (:p (:princ (ruinwesen::text-to-html2 (bknr.text:article-text post))))))))

(defun print-post-short (post)
  (html (:h5 (:princ-safe (bknr.text:article-subject post))
	     ((:span :class "blogdate") (:princ-safe (format-date-time (bknr.text:article-time post)))))
	(html ((:a :name (format nil "post-~A" (store-object-id post))) " "))
	(if (and (typep post 'ruinwesen::rw-blog-article)
		 (not (null (ruinwesen::rw-blog-article-short post))))
	    (html (:p (:princ (ruinwesen::text-to-html2 (ruinwesen::rw-blog-article-short post))))
		  (if (string-equal (bknr.text:article-text post) "")
		      (html (:br))
		      (html (:p ((:a :href (format nil "/blog?id=~A" (store-object-id post))) "Read more ...")))))
	    (html (:p (:princ (ruinwesen::text-to-html2 (bknr.text:article-text post))))))))

(define-bknr-tag news-ticker ()
  (with-query-params ((start "0") (count "5"))
    (setf count (parse-integer count))
    (setf start (parse-integer start))
    (if (> count 20)
	(setf count 20))
    (let* ((all-posts (bknr.text:blog-articles (bknr.text:blog-with-name "ruinwesen-news")))
	   (num-posts (length all-posts)))
      
      (dolist (post (ruinwesen::news-posts :count count :start start))
	(print-post-short post))
      (html (:p
	     (unless (> (+ count start) num-posts)
	       (html ((:a :style "float: left" :href (format nil "/index?start=~A&count=~A"
							     (min (1- num-posts) (+ start count)) count))
		      "<<< Older news")))
	     " " 
	     (when (> start 0)
	       (html ((:a :style "float: right" :href (format nil "/index?start=~A&count=~A"
							      (max 0 (- start count)) count))
		      "Recent news >>>"))))))))
    

(define-bknr-tag blog-mainpage ()
  (with-query-params ((id nil)  (month nil) (year nil) (start "0") (count "5"))
    (setf count (parse-integer count))
    (setf start (parse-integer start))
    (cond ((not (null id))
	   (let ((post (store-object-with-id (parse-integer id))))
	     (print-post post)))

	  (t
	   (if (> count 20)
	       (setf count 20))
	   (let* ((all-posts (bknr.text:blog-articles (bknr.text:blog-with-name "ruinwesen-blog")))
		  (num-posts (length all-posts)))
	     
	     (dolist (post (ruinwesen::rw-posts :count count :start start))
	       (print-post-short post))
	       (html (:p
		      (unless (> (+ count start) num-posts)
			(html ((:a :style "float: left" :href (format nil "/blog?start=~A&count=~A"
						 (min (1- num-posts) (+ start count)) count))
			       "<<< Older posts")))
		      " " 
		      (when (> start 0)
			(html ((:a :style "float: right" :href (format nil "/blog?start=~A&count=~A"
						 (max 0 (- start count)) count))
			       "Newer posts >>>")))))))
	   )))

(define-bknr-tag contact ()
  (with-query-params ((city "")  (name "") (email "") (text ""))
    (let ((errors ruinwesen::*contact-error*))
      (html
       ((:div :id "page-wrap")
	((:div :id "contact-area")
	 (if errors
	     (progn
	       (dolist (error errors)
		 (html ((:p :style "color:#af2d6a;font-weight:bold") (:princ-safe error))))
	       (html (:br))))
	       
	((:form :action "/submit-contact" :method "post")
	 ((:label :for "Name" :id "Name") "Name:")
	 ((:input :type "text" :name "name" :value name))

	 ((:label :for "City" :id "City") "City:")
	 ((:input :type "text" :name "city" :value city))

	 ((:label :for "Email" :id "Email") "Email:")
	 ((:input :type "text" :name "email" :value email))

	 ((:label :for "Message" :id "Message") "Message:") (:br)
	 ((:textarea :name "text" :rows "20" :cols "2)")
	  (if text
	      (html (:princ-safe text))
	      (html " ")))

	 ((:input :type "submit" :name "submit" :value "Submit" :class "submit-button")))
	 ((:div :style "clear: both;"))))))))

(define-bknr-tag radio-button (name label choices &optional value)
  (html ((:label :for name :id name) (:princ-safe label))
	 (dolist (choice choices)
	   (let ((choice-name (first choice))
		 (choice-label (second choice)))
	     (html ((:div :class "radiochoice")
		    (html (:princ-safe choice-label))
		    (if (eql value choice-name)
			(html ((:input :type "radio" :checked "checked" :name name
				       :value choice-name)))
			(html ((:input :type "radio" :name name :value choice-name))))))))))


(define-bknr-tag preorder ()
  (with-query-params ((name "") (email "") (text "") (form-factor "") (device "")
		      (country ""))
    (let ((errors ruinwesen::*preorder-error*))
      (html
       ((:div :id "page-wrap")
	((:div :id "preorder")
	 (if errors
	     (progn
	       (dolist (error errors)
		 (html ((:p :style "color:#af2d6a;font-weight:bold") (:princ-safe error))))
	       (html (:br))))
	       
	((:form :action "/submit-preorder" :method "post")
	 ((:label :for "Name" :id "Name") "Name:")
	 ((:input :type "text" :name "name" :value name))

	 ((:label :for "Country" :id "Country") "Country:")
	 ((:input :type "text" :name "country" :value country))

	 ((:label :for "Email" :id "Email") "Email:")
	 ((:input :type "text" :name "email" :value email))

	 (radio-button "device" "Device: " '(("midicommand" "MiniCommand:  ")
				  ("monojoystick" "MonoJoystick:  ")) device)

	 ((:input :type "hidden" :name "form-factor" :value "mk1"))
	 #+nil(radio-button "form-factor" "Form Factor: " '(("mk1" "MK1: ")
				       ("mk2" "MK2: ")) form-factor)

	 ((:label :for "Message" :id "Message") "Message:") (:br)
	 ((:textarea :name "text" :rows "20" :cols "2)")
	  (if text
	      (html (:princ-safe text))
	      (html " ")))

	 ((:input :type "submit" :name "submit" :value "Submit" :class "submit-button")))
	 ((:div :style "clear: both;"))))))))

(defun list-to-hidden-inputs (list)
  (loop for (var . val) in list
       do (html ((:input :type "hidden" :name var :value val)))))

(defparameter *paypal-cancel-url*
  (puri:render-uri (puri:merge-uris "/cancelpaypal" *website-url*) nil))

(defparameter *paypal-return-url*
  (puri:render-uri (puri:merge-uris "/returnpaypal" *website-url*) nil))

(defun round-to (num to)
  (* to (round num to)))

(define-bknr-tag product-paypal (&key name amount)
  (html ((:form :action "https://www.sandbox.paypal.com/us/cgi-bin/webscr"
		:method "post")
	 
	 (list-to-hidden-inputs `(("cmd" . "_xclick")
				  ("business" . "foobar_1206833805_biz@bl0rg.net")
				  ("currency_code" . "EUR")
				  ("item_name" . ,name)
				  ("amount" . ,amount)
				  ("cancel_return" . ,*paypal-cancel-url*)
				  ("return" . ,*paypal-return-url*)))
	 ((:div :class "purchasebar")
	  ((:input :type "submit"
		   :src "http://www.sandbox.paypal.com/en_US/i/btn/x-click-but01.gif"
		   :name "submit"
		   :value "Click Here to Purchase"
		   :alt "Paypal"))))))

(define-bknr-tag google-analytics ()
  (html (:princ
	 "<script type=\"text/javascript\">
var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
</script>
<script type=\"text/javascript\">
var pageTracker = _gat._getTracker(\"UA-5517945-1\");
pageTracker._trackPageview();
</script>"))) 
