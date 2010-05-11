(in-package :ruinwesen)

(defclass atom-handler (prefix-handler)
  ())

#|
<introspection xmlns="http://purl.org/atom/ns#" > 
  <search-entries>http://example.com/myblog/atom.cgi/search</search-entries>
  <create-entry>http://example.com/myblog/atom.cgi/edit</create-entry>
  <edit-template>http://example.com/atom.cgi/templates</edit-template>
  <user-prefs>http://example.com/myblog/atom.cgi/prefs</user-prefs>
  <categories>http://example.com/atom.cgi/categories</categories>
</introspection>
|#

(defun get-first-node (node)
  (let ((children (dom:child-nodes node)))
    (dotimes (i (length children))
      (let ((node (aref children i)))
	(when (dom:element-p node)
	  (return-from get-first-node node))))))

(defun get-first-node-name (node)
  (dom:node-name (get-first-node node)))

(defun get-first-element-by-name (doc name)
  (aref (dom:get-elements-by-tag-name doc name) 0)) 

(defun get-method-name (doc)
  (get-text-body (get-first-element-by-name doc "methodName")))

(defun get-method-params (doc)
  (loop for node across (dom:get-elements-by-tag-name doc "param")
       collect (parse-xml-rpc (get-first-node node))))

(defun parse-xml-rpc-type (node)
  (let ((type (dom:node-name node)))
    (cond ((string-equal type "string")
	   (get-text-body node))
	  (t (error "unknown type ~A" type)))))

(defun parse-member (member)
  (let ((name (get-text-body (aref (dom:get-elements-by-tag-name member "name") 0)))
	(value (parse-xml-rpc (aref (dom:get-elements-by-tag-name member "value") 0))))
    (cons name value)))

(defun parse-xml-rpc (doc)
  (let* ((node (get-first-node doc))
	 (type (dom:node-name node)))
    (cond
      ((string-equal type "methodCall")
       (cons (get-method-name doc)
	     (get-method-params doc)))
      ((string-equal type "string")
       (get-text-body node))
      ((string-equal type "array")
       (loop for child across (dom:child-nodes (aref (dom:get-elements-by-tag-name node
									       "data") 0))
	    when (dom:element-p child)
	  collect (parse-xml-rpc child)))
      ((string-equal type "i4")
       (parse-integer (get-text-body node)))
      ((string-equal type "boolean")
       (let ((text (get-text-body node)))
	 (string-equal text "0")))
      ((string-equal type "dateTime.iso8601")
       (get-universal-time)) ;; quick hack
      ((string-equal type "struct")
       (loop for child across (dom:child-nodes node)
	    when (and (dom:element-p child)
		      (string-equal (dom:node-name child) "member"))
	    collect (parse-member child)))
      (t (error "unknown type ~A" type)))))

(defclass iso8601-time ()
  ((time :initarg :time :reader iso8601-time)))

(defmethod time-to-string ((time iso8601-time))
  (multiple-value-bind (sec min hour day month year )
      (decode-universal-time (iso8601-time time))
    (format nil "~A~2,'0D~2,'0DT~2,'0D:~2,'0D:~2,'0D" year month day hour min sec)))
  

(defclass rpc-struct ()
  ((vars :initarg :vars :reader rpc-struct-vars)))

(defmethod print-object ((struct rpc-struct) stream)
  (print-unreadable-object (struct stream :type t)
    (format stream "~S" (rpc-struct-vars struct))))

(defun xml-rpc-encode-list (obj)
  (cond ((stringp obj)
	 `(:value (:string ,obj)))
	((eql obj :bool-false)
	 `(:value (:boolean 1)))
	((eql obj :bool-true)
	 `(:value (:boolean 0)))
	((typep obj 'rpc-struct)
	 `(:value (:struct ,@(loop for elt in (rpc-struct-vars obj)
				collect `(:member (:name ,(car elt))
						  ,(xml-rpc-encode-list (cdr elt)))))))
	((listp obj)
	 `(:value (:array (:data ,@(mapcar #'xml-rpc-encode-list obj)))))
	((typep obj 'iso8601-time)
	 `(:value ("dateTime.iso8601" ,(time-to-string obj))))
	((numberp obj)
	 `(:value (:int ,(princ-to-string obj))))
	(t (error "unknown type"))))

(defun xml-rpc-encode (obj)
  (list-to-xml (xml-rpc-encode-list obj)))

(defun make-struct (vars)
  (make-instance 'rpc-struct :vars vars))

(defun get-user-info (param1 username password)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  
  (let ((user (find-user username)))
    (make-struct
     `(("userid" . ,(store-object-id user))
       ("firstname" . ,(user-full-name user))
       ("lastname" . ,(user-full-name user))
       ("nickname" . ,(user-login user))
       ("email" . ,(user-email user))
       ("url" . "")))))

(defun get-template ()
  )

(defun set-template ()
  )

(defun edit-post (appkey postid username password content publish)
  )

(defun new-post (appkey blogid username password content publish)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  ;; verify access to blog

  (let* ((id (parse-integer blogid))
	 (blog (store-object-with-id id)))
    (let ((article (make-instance 'rw-blog-article
				:text content
				:author (find-user username))))
    (bknr.text::blog-add-article blog article)
    
    (princ-to-string (store-object-id article)))))

(defun struct-field (struct name)
  (cdr (assoc name struct :test #'string-equal)))

(defun mt-new-post (blogid username password struct publish)
;;  (format t "HAHA ~A ~A ~A ~A~%" username password blogid struct)

  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  ;; verify access to blog

  (let* ((id (parse-integer blogid))
	 (blog (store-object-with-id id)))
    (format t "blog: ~A~%" blog)
    (let ((article (make-object 'rw-blog-article
				:short (struct-field struct "description")
				:subject (struct-field struct "title")
				:text (struct-field struct "mt_text_more")
				:author (find-user username))))
    (bknr.text::blog-add-article blog article)
    
    (princ-to-string (store-object-id article)))))

(defun mt-edit-post (postid username password struct publish)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))

  (let* ((id (parse-integer postid))
	 (post (store-object-with-id id)))
    (with-transaction ()
      (let ((description (struct-field struct "description"))
	    (text (struct-field struct "mt_text_more"))
	    (title (struct-field struct "title")))
	(when description
	  (setf (rw-blog-article-short post) description))
	(when text
	  (setf (article-text post) text))
	(when title
	  (setf (article-subject post) title))))
    :bool-true))

(defun get-post (appkey postid username password)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  
  (let ((id (parse-integer postid)))
    (article-struct (store-object-with-id id))))

(defun mt-get-post (postid username password)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  
  (let ((id (parse-integer postid)))
    (article-struct (store-object-with-id id))))

(defun article-struct (article)
  (make-struct
   `(("description" .  ,(or (rw-blog-article-short article) ""))
     ("postid" . ,(princ-to-string (store-object-id article)))
     ("userid" . ,(princ-to-string (store-object-id (article-author article))))
     ("title" . ,(article-subject article))
     ("dateCreated" . ,(make-instance 'iso8601-time :time (article-time article)))
     ("link" . ,(rss-item-link article))
     ("permaLink" . ,(rss-item-link article))
     ("mt_allow_pings" . 0)
     ("mt_allow_comments" . 0)
     ("mt_convert_breaks" . "__default__")
     ("mt_excerpt" . ,(or (rw-blog-article-short article) ""))
     ("mt_text_more" . ,(or (article-text article) ""))
     ("mt_keywords" . ""))))
     
(defun get-recent-posts (param1 blogid username password num)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  
  (let* ((id (parse-integer blogid))
	 (blog (store-object-with-id id)))
    (mapcar #'(lambda (x)
		(article-struct x))
	    (blog-posts blog :count num)))) ;;; num))))

(defun mt-get-recent-posts ( blogid username password num)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  
  (let* ((id (parse-integer blogid))
	 (blog (store-object-with-id id)))
    (mapcar #'(lambda (x)
		(article-struct x))
	    (blog-posts blog :count num)))) ;;num))))
	 
    
  ;; link, permaLink, userid, mt_allow_pings, mt_allow_comments, description,
  ;; mt_convert_breaks, postid, mt_excerpt, mt_keywords, title, mt_text_more,
  ;; dateCreated

(defun delete-blog-post (article)
  (let ((blog (bknr.text::blog-article-blog article)))
    (with-transaction ()
      (setf (blog-articles blog)
	    (remove article (blog-articles blog)))
      #+nil(delete-object article))))

(defun clean-blog-posts (blog)
  (dolist (article (blog-articles blog))
    (when (object-destroyed-p article)
      (with-transaction ()
	(setf (blog-articles blog)
	      (remove article (blog-articles blog)))))))
	

(defun delete-post (param1 postid username password publish)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  
  (let* ((id (parse-integer postid))
	 (object (store-object-with-id id)))
    (if (typep object 'rw-blog-article)
	(progn
	  (delete-blog-post (store-object-with-id id))
	  :bool-true)
	:bool-false)))

(defun get-post-categories (postid username password)
  nil)

(defun set-post-categories (postid username password categories)
  :bool-true)

(defun mt-supported-text-filters ()
  nil)

(defun get-user-blogs (param1 username password)
  (unless (verify-password (find-user username) password)
    (error "Could not verify password"))
  (loop for blog in (store-objects-with-class 'blog)
       collect (make-struct `((:blogid . ,(store-object-id blog))
			      ("blogName" . ,(blog-name blog))
			      (:url . ,(bknr.rss::rss-channel-path blog))))))

(defun mt-get-categories (blogid username password)
  nil)


(defparameter *blogger-requests* nil)

(defun blogger-dispatch (request)
;;  (push request *blogger-requests*)
  (let ((fun (cdr (assoc (first request) *blogger-dispatch* :test #'string-equal))))
    (if fun
      (apply (symbol-function fun) (cdr request))
      (error "Could not dispatch ~A" (first request)))))

(defun mt-get-category-list (blogid username password)
  (list (make-struct `(("categoryId" . "0")
		       ("categoryName" . "default")))))

(defparameter *blogger-dispatch*
  `(("blogger.getUsersBlogs" . get-user-blogs)
    ("blogger.getUserInfo" . get-user-info)
    ("blogger.getTemplate" . get-template)
    ("blogger.setTemplate" . set-template)
    ("blogger.editPost" . edit-post)
    ("blogger.newPost" . new-post)
    ("blogger.getPost" . get-post)
    ("blogger.getRecentPosts" . get-recent-posts)
    ("blogger.deletePost" . delete-post)
    ("mt.supportedTextFilters" . mt-supported-text-filters)
    ("mt.getCategoryList" . mt-get-category-list)
    ("mt.getPostCategories" . get-post-categories)
    ("mt.setPostCategories" . set-post-categories)
    ("metaWeblog.getPost" . mt-get-post)
    ("metaWeblog.newPost" . mt-new-post)
    ("metaWeblog.editPost" . mt-edit-post)
    ("metaWeblog.getRecentPosts" . mt-get-recent-posts)
    ("metaWeblog.getCategories" . mt-get-categories)
    ))

(defmethod handle ((handler atom-handler))
  #+nil(format t "~A~%" 
	  (raw-post-data :force-text t))
  (let* ((doc (cxml:parse-octets (raw-post-data) (cxml-dom:make-dom-builder))))
    (setf *doc* doc)
    (let (
	 (request (parse-xml-rpc doc)))
    (handler-case
	(list-to-xml
	 `("methodResponse"
	   (:params
	    (:param ,(xml-rpc-encode-list (blogger-dispatch request))))))
      (error (e)
	(list-to-xml
	 `("methodResponse"
	   (:fault
	    ,(xml-rpc-encode-list `(("faultCode" . 0)
				    ("faultString" . ,(princ-to-string e))))))))))))

(defun list-to-xml (list)
  (with-output-to-string (stream)
    (let ((sink (make-character-stream-sink stream))) ; :omit-xml-declaration-p t)))
      (with-xml-output sink
	(list-to-xml-2 list stream)))))

(defun xml-name-to-string (elt)
  (cond ((keywordp elt)
	 (coerce (string-downcase (symbol-name elt))
		 '(simple-array character (*))))
	((stringp elt)
	 elt)
	(t (error "unknown xml type ~A" elt))))
  

(defun list-to-xml-2 (list stream)
  (cond ((listp list)
	 (with-element (xml-name-to-string (first list))
	   (dolist (elt (cdr list))
	     (list-to-xml-2 elt stream))))
	((keywordp list)
	 (cxml:text (string-downcase (symbol-name list))))
	(t (cxml:text (princ-to-string list)))))
