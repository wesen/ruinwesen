(in-package :md)

(enable-prototype-syntax)
(enable-interpol-syntax)

(defparameter *host-prefix* "http://localhost:4242")
(defparameter *elektron-prefix* "/md/")

(defparameter *elektron-dir* "/home/manuel/elektron-dir/")
(defparameter *image-cache-dir* (merge-pathnames
				 (make-pathname :directory '(:relative "image-cache"))
				 *elektron-dir*))

(defparameter *logfile* "/tmp/elektron.log")

(defvar *elektron-server*)
(defvar *dispatch-table*)

(defparameter *elektron-css-file* "/files/elektron.css")

(defvar *main-page-uri* "/index")
(defvar *files-uri* "/files/")
(defvar *ajax-uri* "/ajax")

(defun param-map (str assoc)
  (cdr (assoc str assoc :test #'string-equal)))

(defgeneric object-to-html (object))

(defmacro md-img-text (text &optional scale &rest foobar)
  `(with-html-output (*standard-output* nil)
     (:img :src (format nil "/text-image/~a~A" ,text ,(if scale (format nil "?scale=~A" scale) ""))
	   :alt ,text ,@foobar)))

(defmethod object-to-html ((kit kit))
  (with-html-output-to-string (*standard-output* nil)
    ((:div :class "mdobject" :id (format nil "md-~A" (store-object-id kit)))
     (md-img-text "KIT" 2 :class "handle") :br
     (md-img-text (md-object-name kit)) :br
     (when (md-object-user kit)
       (md-img-text (md-user-name (md-object-user kit)))))))

(defmethod object-to-html ((song md-song))
  (with-html-output-to-string (*standard-output* nil)
     ((:div :class "mdobject" :id (format nil "md-~A" (store-object-id song)))
      (md-img-text "SONG" 2 :class "handle") :br
    ((:a :href (format nil "/image/song/~a?scale=2"
		       (store-object-id song))
	 :rel "lightbox[song]")
     (md-img-text (md-object-name song))) :br
      (when (md-object-user song)
	(md-img-text (md-user-name (md-object-user song)))))))

(defmethod object-to-html ((pattern md-pattern))
  (with-html-output-to-string (*standard-output* nil)
    ((:div :class "mdobject" :id (format nil "md-~A" (store-object-id pattern)))
     (md-img-text "PATTERN" 2 :class "handle") :br
     (md-img-text (symbol-name (pattern-name (md-pattern-position pattern)))) :br
     (md-img-text (if (md-pattern-kit pattern)
		      (md-object-name (md-pattern-kit pattern))
		      "NO KIT")) :br
     (when (md-object-user pattern)
       (md-img-text (md-user-name (md-object-user pattern)))))))
  
(defmethod object-to-html ((machine machine))
  (with-html-output-to-string (*standard-output* nil)
    ((:div :class "mdcontainer"
	   :onmouseover (format nil "javascript:showMdobjectFunctions($(\"func-~A\"), ~A)"
				(store-object-id machine)
				(store-object-id machine))
	   )
      ((:div :class "mdobject" :id (format nil "md-~A" (store-object-id machine)))
	    (md-img-text "MACHINE" 2 :class "handle") :br
	    ((:a :href (format nil "/image/machine/~a?scale=2"
			       (store-object-id machine))
		 :rel "lightbox[machine]")
	     (md-img-text (machine-name (machine-model machine))) :br
	     (md-img-text (if (machine-kit machine)
			      (md-object-name (machine-kit machine))
			      "NO KIT"))) :br
			      (when (md-object-user machine)
				(md-img-text (md-user-name (md-object-user machine)))))
     ((:div :id (format nil "func-~A" (store-object-id machine)))))))
  
(defmethod object-to-html ((user md-user))
  (with-html-output-to-string (*standard-output* nil)
    ((:div :class "mdobject" :id (format nil "md-~A" (store-object-id user)))
     (md-img-text "USER" 2 :class "handle") :br
     (md-img-text (md-user-name user)))))

(defmethod object-to-html ((import md-import))
  (with-html-output-to-string (*standard-output* nil)
    ((:div :class "mdobject" :id (format nil "md-~A" (store-object-id import)))
     (md-img-text "SYSEX" 2 :class "handle") :br
     (md-img-text (md-object-name import)) :br
     (when (md-object-user import)
       (md-img-text (md-user-name (md-object-user import)))))))

(defun start-distance (idx start count)
  (abs (floor (/ (- start (* idx count)) count))))

(defun html-search-nav (start count length)
  (with-html-output-to-string (*standard-output* nil)
    ((:div :class "searchnav")
     (md-img-text "Results:" 2)
     (loop for idx from 0
	for strt from 0 by count upto length
	for end = (+ strt count)
	for distance = (start-distance idx start count)
	do (cond ((= distance 0)
		  (md-img-text idx 2) (princ " "))
		 ((or (< distance 4)
		      (= idx 0)
		      (< (start-distance idx length count) 1))
		  (with-html-output (*standard-output* nil)
		    ((:a :href "#" :onclick (format nil "javascript:searchResults(~A, ~A)"
						    strt count))
		     (md-img-text idx 2)) " "))
		 ((< distance 5)
		  (md-img-text "..." 2) (princ " ")
		  ))))))
		 

(defun html-object-list (objects &key (count 32) (start 0))
  (with-output-to-string (s)
    (let ((length (length objects)))
      (with-html-output (s nil)
	(:div 
	 (dolist (obj (subseq objects start (min length (+ start count))))
	   (princ (object-to-html obj) s)
	   (princ #\Newline s))))
      (princ (html-search-nav start count length) s))))

(defun-ajax md-list (type (start "0") (count "32"))
  (let* ((class (param-map type '(("songs" . md-song)
				  ("song" . md-song)
				  ("patterns" . md-pattern)
				  ("pattern" . md-pattern)
				  ("kits" . kit)
				  ("kit" . kit)
				  ("machines" . machine)
				  ("machine" . machine)
				  ("sysex" . md-import))))
	   (start (or (parse-integer start :junk-allowed t) 0))
	   (count (or (parse-integer count :junk-allowed t) 32)))
    ;;; check rights XXX
      (html-object-list (store-objects-with-class class) :count count :start start)))

(defun-ajax md-search (query type (count "32") (start "0"))
    (let* ((type2 (param-map type '(("songs" . "song")
				    ("patterns" . "pattern")
				    ("kits" . "kit")
				    ("machines" . "machine")
				    ("sysex" . "import"))))
	   (start (or (parse-integer start :junk-allowed t) 0))
	   (count (or (parse-integer count :junk-allowed t) 32))
	  (res (montezuma-get (format nil "+\"~A\" +type:\"~A\"" query type2)
			      :num-docs count :first-doc start)))
      ;;; check rights XXX
      (html-object-list (mapcar #'first (sort res #'> :key #'second)) :count 20000)))

(defun extract-id (id)
  (when id
    (multiple-value-bind (match strings)
	(scan-to-strings #?r"md-([0-9]+)" id)
      (when match
	(parse-integer (aref strings 0))))))

(defun user-get-object (id)
  (store-object-with-id id))

;; XXX ajax error handling
(defun-ajax add-to-dropbox (id)
  (let ((id (extract-id id)))
    (format t "parsed id ~A~%" id)
    (when id
      (let ((obj (user-get-object id)))
	(when (< (length (session-value 'dropbox)) 64)
	  (pushnew obj (session-value 'dropbox))))))
  "")

(defun-ajax remove-from-dropbox (id)
  (let ((id (extract-id id)))
    (format t "parsed id ~A~%" id)
    (when id
      (let ((obj (user-get-object id)))
	(when (member obj (session-value 'dropbox))
	  (setf (session-value 'dropbox)
		(remove obj (session-value 'dropbox)))))))
  "")

(defun-ajax dropbox ()
  (html-object-list (session-value 'dropbox)))

(defun-ajax stored ()
  (html-object-list nil))

(defun start-elektron (&key port)
  (setf (hunchentoot:log-file) (make-pathname :defaults *logfile*))
  (ensure-directories-exist *elektron-dir*)
  (setf *elektron-server*
	(hunchentoot:start-server
	 :port port
	 :dispatch-table
	 (list (hunchentoot:create-prefix-dispatcher *main-page-uri* 'md-main-page)
	       (hunchentoot:create-prefix-dispatcher "/test" 'test-page)
	       (hunchentoot:create-prefix-dispatcher "/bla" 'test2-page)
	       (create-function-dispatcher "/ajax/" (list 'ajax-test 'md-list
							  'md-search
							  'add-to-dropbox 'remove-from-dropbox
							  'dropbox
							  'stored))
	       (hunchentoot:create-prefix-dispatcher "/image/" 'image-page)
	       (hunchentoot:create-prefix-dispatcher "/text-image/" 'text-image-page)
	       (hunchentoot:create-folder-dispatcher-and-handler
		*files-uri*
		(make-pathname :defaults *elektron-dir*))
	      'md-main-page))))

(defun stop-elektron ()
  (hunchentoot:stop-server *elektron-server*))

(defun test2-page ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    ((:html :xmlns "http://www.w3.org/1999/xhtml" "en" :lang "en")
     (:head (:title "ELEKTRON MD CONVERTER")
	    #+nil(str (generate-prologue *ajax-processor*))
	    (:script :type "text/javascript" :src "/files/js/prototype.js")
	    (:script :type "text/javascript" :src "/files/js/scriptaculous.js")
	    (:script :type "text/javascript" :src "/files/js/lightbox.js")
	    (:link :href "/files/lightbox.css" :rel "stylesheet" :type "text/css"))
     (:body
      ((:a :href "/text-image/foo?scale=4" :rel "lightbox")
       (:img :src "/text-image/foo"))))))

(defun test-page ()
  (start-session)
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    ((:html :xmlns "http://www.w3.org/1999/xhtml" "en" :lang "en")
     (:head (:title "ELEKTRON MD CONVERTER")
	    #+nil(str (generate-prologue *ajax-processor*))
	    (:script :type "text/javascript" :src "/files/js/prototype.js")
	    (:script :type "text/javascript" :src "/files/js/scriptaculous.js")
	    (:script :type "text/javascript" :src "/files/js/lightbox.js")
	    (:link :href *elektron-css-file* :rel "stylesheet" :type "text/css")
	    (:link :href "/files/lightbox.css" :rel "stylesheet" :type "text/css")
	    (js-script
	     (defvar search-type "kits")
	     (defvar search-style "")
	     (defvar search-query "")
	     (defvar search-ajax nil)
	     (defvar result-drags $A(list))
	     
	     (defun show-select ()
	       (.show $("select"))
	       (setf (slot-value $("q") 'value) ""))

	     (defun hide-select ()
	       (.hide $("select")))

	     (defun select (type)
	       (let ((elt $(type))
		     (sels $$(".selected")))
		 (when sels
		   (sels.each  (lambda (x) (setf x.class-name ""))))
		 (setf search-type type)
		 (setf elt.class-name "selected")
		 (show-select)
		 (list-type type)))

	     (defun show-help (message)
	       (new (-effect.-fade $("help") (create :duration 0.3
						     :after-finish (lambda ()
								     (setf (slot-value $("help") 'inner-h-t-m-l)
									   message)))))
	       (new (-effect.-appear $("help")
				     (create :duration 0.3 :queue "end")))
		 
	       )

	     (defun show-hint (message)
	       (let ((elt $("help")))
		 (unless elt.old-messages
		   (setf elt.old-messages (list)))
		 (elt.old-messages.push elt.inner-h-t-m-l)
		 (setf elt.inner-h-t-m-l message)))

	     (defun hide-hint ()
	       (let ((elt $("help")))
		 (when elt.old-messages
		   (let ((message (elt.old-messages.pop)))
		     (when message
		       (setf elt.inner-h-t-m-l message))))))

	     (defun open-lightbox(anchor)
	       (unless (= (typeof my-lightbox)
			  "undefined")
		 (my-lightbox.start anchor)))
	       
	       
	     (defun show-mdobject-functions (elt id)
	       ;; dock style XXX
	       (let ((funcs $("mdobjectfuncs")))
		 (unless (and funcs
			      (not (= (.index-of $A((elt.child-elements)) funcs) -1)))
		   (when funcs
		     (funcs.remove))
		   (+= elt.inner-h-t-m-l (html ((:div :id "mdobjectfuncs" :style "display:none")
						  ((:img :src "/text-image/EDIT"))
						" "
						((:a :href (+ "/image/object/" id "?scale=2")
						     :onclick (js-inline
							       (open-lightbox this)
							       (return false)))
						 ((:img :src "/text-image/VIEW"))))))
		   (-effect.-blind-down "mdobjectfuncs" (create :duration 0.5)))))
	       
	     (defun hide-mdobject-functions ()
	       (let ((elt $("mdobjectfuncs")))
		 (when elt
		   (new (-effect.-blind-up elt (create :duration 0.5
						       :after-finish (lambda () (elt.remove))))))))
	       
	     (defun page-load-finished ()
	       (select "kits"))
	     
	     (defun add-to-dropbox (id)
	       (new (-ajax.-request "/ajax/add-to-dropbox"
				    (create :method "post"
					    :parameters (create :id id)
					    :asynchronous t
					    :on-success (lambda (transport)
							  (when (= search-type "dropbox")
							    (list-type "dropbox")))))))

	     (defun remove-from-dropbox (elt)
	       (new (-ajax.-request "/ajax/remove-from-dropbox"
				    (create :method "post"
					    :parameters (create :id elt.id)
					    :asynchronous t
					    :on-success (lambda (transport)
							  (new (-effect.-puff
								elt
								(create :duration 0.5)))
							  #+nil(list-type "dropbox"))))))
	     

	     (defun refresh-draggables ()
	       (.each result-drags (lambda (x) (.destroy x)))
	       (setf result-drags $A(list))
	       (.each $$(".mdobject")
		      (lambda (x) (let ((drag
					 (new (-Draggable x.id
							  (create :revert t
								  :handle "handle")))))
				    (.push result-drags drag))))
	       (show-help "Drag and drop items by dragging on their title. Add an item to your dropbox by dragging it over the dropbox item.")
	       )
	       
	       
	     (defun ajax-results (url params)
	       (unless params
		 (setf params (create)))
	       (setf params.on-complete
		     refresh-draggables)
	       (new (-Ajax.-Updater "results" url params)))

	     (defun search-results (start count)
	       (cond ((= search-style "list")
		      (list-type search-type start count))
		     ((= search-style "search")
		      (search search-query start count))))
	     
	     (defun list-type (type (start 0) (count 32))
	       (setf search-style "list")
	       (cond
		 ((= type "dropbox")
		  (ajax-results "/ajax/dropbox"))
		 ((= type "stored")
		  (ajax-results "/ajax/stored"))
		 (t (ajax-results  "/ajax/md-list"
				   (create :parameters
					   (create :type search-type
						   :start start
						   :count count))))))
	     
	     (defun search ((my-search-query (slot-value $("q") 'value))
			    (start 0) (count 32))
	       (setf search-style "search")
	       (setf search-query my-search-query)
	       (ajax-results  "/ajax/md-search"
			      (create :parameters
				      (create :type search-type
					      :query search-query
					      :start start
					      :count count))))
	       ))

     
	    
     ((:body :bgcolor "#ffffff" :color "#000000" :onload (js:js-inline (page-load-finished)))
      ((:div :id "container")
       ((:div :id "categories")
	((:ul :class "nav")
	 ((:li :id "sysex")
	  ((:a :href "#" :onclick (js:js-inline (select "sysex")))
	   (md-img-text "SYSEX" 2)))
	 ((:li :id "kits")
	  ((:a :href "#" :onclick (js:js-inline (select "kits")))
	   (md-img-text "KITS" 2)))
	 ((:li :id "machines")
	  ((:a :href "#" :onclick (js:js-inline (select "machines")))
	   (md-img-text "MACHINES" 2)))
	 ((:li :id "songs")
	  ((:a :href "#" :onclick (js:js-inline (select "songs")))
	   (md-img-text "SONGS" 2)))
	 ((:li :id "wavs")
	  ((:a :href "#" :onclick (js:js-inline (select "wavs")))
	   (md-img-text "WAVS" 2)))
	 ((:li :id "midis")
	  ((:a :href "#" :onclick (js:js-inline (select "midis")))
	   (md-img-text "MIDIS" 2)))
	 ((:li :id "users")
	  ((:a :href "#" :onclick (js:js-inline (select "users")))
	   (md-img-text "USERS" 2)))
	 ((:li :id "stored")
	  ((:a :href "#" :onclick (js:js-inline (select "stored")))
	   (md-img-text "STORED" 2)))
	 ((:li :id "dropbox")
	  ((:a :href "#" :onclick (js:js-inline (select "dropbox")))
	   (md-img-text "DROPBOX" 2)))
	 ((:li :id "trash")
	  (md-img-text "TRASH" 2))))
	  

       (js-script (-droppables.add "trash"
				   (create :on-drop (lambda (drag drop evt)
						      (when (= search-type "dropbox")
							(remove-from-dropbox drag)
							(new (-effect.-pulsate
							      drop
							      (create :pulses 2
								      :duration 0.5
								      :from 0.2))))
							)
					   :hoverclass "drophover")))
       
       (js-script (-droppables.add "dropbox"
				   (create :on-drop (lambda (drag drop evt)
						      (add-to-dropbox drag.id)
						      (new (-effect.-pulsate
							    drop
							    (create :pulses 2
								    :duration 0.5
								    :from 0.2))))
					   :hoverclass "drophover")))

       ((:div :id "select")
	(:form :action (js:js-inline (search))
	 (:input :class "simage" :type "image"
		 :src "/text-image/search:?scale=2")
	 (:input :id "q" :type "search" :name "q")))
       ((:div :id "helpholder")
	((:div :id "help") "Welcome to MD Editor"))
       
       ((:div :id "results"))

       )))))


		       
		       
(defmacro with-elektron-page ((&rest params) &rest body)
  `(with-html-output-to-string (,@params)
     (:html
      (:head (:title "ELEKTRON MD CONVERTER")
	     (:link :rel "shortcut icon" :type "image/x-icon" :href "/files/favicon.ico")
	     #+nil(:link :href *elektron-css-file* :rel "stylesheet" :type "text/css"))
      ((:body :bgcolor "#ffffff" :color "#000000")
       (:div :align :center 
	     :br :br
	     (:img :src "/files/pngs/md-sysex.png" :border 0 :alt "MD SYSEX CONVERTER")
	     :br :br
	     ,@body)))))

(defmacro with-sub-elektron-page ((s title i big &rest params) &rest body)
  `(with-html-output (,s nil :indent t ,@params)
     (:html
      (:head (:title ,title)
	     (:link :rel "shortcut icon" :type "image/x-icon" :href "/files/favicon.ico"))
      ((:body :bgcolor "#ffffff" :color "#000000")
       (:div :align :center ((:a :href (format nil "/files/~A.html" ,i))
			     (:img :src ,(if big "/files/sps1-uw.jpg"
					    "/files/sps1-uw-small.jpg")
				   :border 0
				   :alt "image of sps1 uw (c) hageir")
			     :br :br
			     (:img :src ,(if big `(format nil "/files/filename-~A.gif" ,i)
					     "/files/pngs/back.png")
				   :border 0
				   :alt ,(if big "FILENAME" "BACK")))
	     :br :br
       ,@body)))))


(defun error-page ()
  (with-elektron-page (*standard-output* nil :prologue t :indent t)
    (:img :src "/files/pngs/sysex-error.png" :border 0
	  :alt "ERROR IN THE SYSEX FILE! PLEASE UPLOAD AGAIN:")
    :br :br
    ((:form :method :post :enctype "multipart/form-data")
     (:input :type "file" :name "test" :maxlength "500000") :br :br
     (:input :type :submit :name "upload" :value "UPLOAD"))))

	       