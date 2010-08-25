(in-package :ruinwesen)

(define-persistent-class rw-patch (blob)
  ((title :update :initform ""
	  :index-type hash-index
	  :index-reader patches-with-title)
   (comment :update :initform "")
   (author :read :initform nil
	   :index-type hash-index
	   :index-reader patches-by-author)
   (tags :update :initform nil
	 :index-type hash-list-index
	 :index-reader patches-with-tag)
   (last-modified :update :initform (get-universal-time))
   (device-id :update :initform nil
	      :index-type hash-index
	      :index-reader patches-for-device)
   (environment-id :update :initform nil
	      :index-type hash-index
	      :index-reader patches-for-environment)
   (downloads :update :initform 0)
   (deleted :update :initform nil)
   (name :update :initform "")))

(defmethod print-object ((patch rw-patch) stream)
  (print-unreadable-object (patch stream :type t)
    (with-slots (title author) patch
      (format stream "ID: ~A ~S by ~A" (store-object-id patch) title author))))

(define-persistent-class rw-patch-comment ()
  ((body :update :initform "")
   (author :read :initform nil
	   :index-type hash-index
	   :index-reader comments-by-author)
   (created :read :initform (get-universal-time))
   (patch :read :initform nil
	  :index-type hash-index
	  :index-reader comments-for-patch)))

;; create patch
;; find patch
;; search patch
;; patch by author
;; patch with tag
;; delete patch
;; latest
;; download anzahl
;; kommentare

(defun all-patches ()
  (store-objects-with-class 'rw-patch))

#+nil(make-object 'rw-patch
	     :author (find-user "wesen")
	     :title "MDWesenLivePatch"
	     :comment "my live patch"
	     :tags '(:machinedrum :live :delay :supatrigga :midiclock)
	     :device-id :minicommand2
	     :name "MDWesenLivePatch")

(defparameter *json-protocol-id* "1.1")
(defparameter *json-client-version* "1.0")
(defparameter *json-client-url* "http://ruinwesen.com/minicommand")

(defun json-action-reply (action status message &optional additional)
  (yason:with-output-to-string* ()
    (yason:with-object ()
      (yason:encode-object-elements
       "protocol-id" *json-protocol-id*
       "action" "response"
       "requested-action" action
       "status" status)
      (when message
        (yason:encode-object-element "message" message))
      (when additional
        (apply #'yason:encode-object-elements additional)))))

(defun patch-check-login (user password)
  (verify-password (find-user user) password))

(defvar *json-auth* nil)

(defun json-username ()
  (and *json-auth*
       (json-get-value :username *json-auth*)))

(defun json-check-auth (&optional _username)
  (when (null *json-auth*)
    (return-from json-check-auth nil))
  (let ((username (json-get-value :username *json-auth*))
	(password (json-get-value :password *json-auth*)))
    (if (or (null username)
	    (null password))
	nil

	(let* ((user (find-user username))
	       (user-verified-p (when user
				  (verify-password user password))))
	  (if _username
	      (and user-verified-p
		   (or (admin-p user)
		       (string= (user-login user)
				_username)))
	      user-verified-p)
	       

	  ))))

(defun json-get-user (&optional _username)
  (let ((auth-p (json-check-auth _username)))
    (when auth-p
      (find-user (json-username)))))

(defun json-create-user (json)
  (let ((user (json-get-value :username *json-auth*))
	(password (json-get-value :password *json-auth*))
	(email (json-get-value :email *json-auth*)))
    (if (or (null user)
	    (null password)
	    (string= user ""))
	(json-action-reply "register-new-user" "failed" "invalid user information")
	(handler-case
	    (progn
	      (let ((user-object (make-user user :password password :email email)))
		(json-action-reply "register-new-user" "ok" (format nil "registered new user ~S" user))))
	  (error (e)
	    (declare (ignore e))
	    (json-action-reply "register-new-user" "failed"
			       (format nil "login ~S already exists" user)))))))

(defun json-get-value (name json)
  (cdr (assoc name json)))

(defun json-get-metadata-value (name json)
  (let ((metadata (json-get-value :patch-metadata json)))
    (when metadata
      (cdr (assoc name metadata)))))

(defun make-patch-from-json (json)
  (let ((data (json-get-value :data json)))
    (when data
      (let ((patch (make-instance 'rw-patch
                                  :author (json-username)
                                  :comment (json-get-metadata-value :comment json)
                                  :title (json-get-metadata-value :title json)
                                  :name (json-get-metadata-value :name json)
                                  :tags (cons :needs-to-be-approved
                                              (mapcar #'make-keyword-from-string
                                                      (json-get-metadata-value :tags json)))
                                  :device-id (json-get-metadata-value :device-id json)
                                  :environment-id (json-get-metadata-value :environment-id json))))
	(handler-case
	    (let ((arr (cl-base64:base64-string-to-usb8-array data)))
	      (blob-from-array patch arr)
	      patch)
	  (error (e)
	    (declare (ignore e))
	    (delete-object patch)
	    nil))))))

(defun patch-metadata-to-json (patch)
  (with-slots (title comment author tags deleted last-modified device-id name) patch
    `((:title . ,title)
      (:id . ,(store-object-id patch))
      (:comment . ,comment)
      (:author . ,author)
      (:tags . ,tags)
      ("last-modified-date" . ,(cybertiggyr-time:format-time nil "%Y-%m-%d" last-modified))
      ("device-id" . ,device-id)
      (:name . ,name))))
  

(defun patch-to-patch-source-json (patch)
  (with-slots (deleted) patch
    `(("patch-id" . ,(store-object-id patch))
      ("patch-url" . ,(format nil "http://ruinwesen.com/patch/~A"
			      (store-object-id patch)))
      ,@(when deleted `((:deleted . ,deleted)))
      ("patch-metadata" . ,(patch-metadata-to-json patch)))))
 
(defun json-upload-patch (json)
  (if (json-check-auth)
      (let ((patch (make-patch-from-json json)))
	(if (null patch)
	    (json-action-reply "store-new-patch" "failed"
			       "could not create/store/decode patch")
	    (json-action-reply "store-new-patch" "ok" nil
			       `(("patch-source" . ,(patch-to-patch-source-json patch))))))
      (json-action-reply "store-new-patch" "failed" "username/password incorrect")))

(defun find-patches-since (date-since &optional (approved t))
  (setf date-since 0)
  (sort (remove-if-not #'(lambda (x)
			   (and (>= (blob-timestamp x) date-since)
				(if approved
				    (not (member :needs-to-be-approved (rw-patch-tags x)))
				    t)))
		       (store-objects-with-class 'rw-patch)) #'< :key #'blob-timestamp))

(defun json-patch-url-list (json)
  (let* ((date-since-str (json-get-value :date-since json))
	 (date-since (if date-since-str
			 (parse-time date-since-str)
			 0))
	 (user (json-get-user)))
    (json-action-reply "get-patch-source-list" "ok" nil
		       `(("patch-source-list"
			  . ,(let ((res (loop for patch in
					     (find-patches-since date-since (if (and user
										     (admin-p user))
										nil
										t))
					   collect (patch-to-patch-source-json patch))))
				  (if res
				      res
				      (make-array 0))))
			 ("date-since" . ,date-since-str)))))

(defun approve-patch (patch)
  (with-transaction ()
    (setf (rw-patch-tags patch)
	  (remove :needs-to-be-approved (rw-patch-tags patch))
	  (slot-value patch 'bknr.datastore::timestamp) (get-universal-time))))

(defun unapprove-patch (patch)
  (with-transaction ()
    (setf (rw-patch-tags patch)
	  (push :needs-to-be-approved (rw-patch-tags patch))
	  (slot-value patch 'bknr.datastore::timestamp) (get-universal-time))))

(defun json-delete-patch (json)
  (let* ((patch-id (parse-integer (or (json-get-value :patch-id json) "") :junk-allowed t))
	 (patch (when patch-id
		  (store-object-with-id patch-id)))
	 (author (when patch
		   (rw-patch-author patch)))
	 (user (json-get-user)))

    (format t "patch: ~A, user: ~A, author: ~A~%" patch user author)

    (cond ((null patch-id)
	   (format t "invalid")
	   (json-action-reply "delete-patch" "failed" "invalid patch id"))
	  ((null patch)
	   (format t "null~%")
	   (json-action-reply "delete-patch" "failed" "no patch with this id"))
	  ((not (or (and user (admin-p user))
		    (json-check-auth author)))
	   (format t "auth ~%")
	   (json-action-reply "delete-patch" "failed"
			      (format nil "no credentials to delete object ~A" patch)))
	  (t
	   (format t "foobar author ~A~%" author)
;;	   (delete-object patch)
	   (with-transaction ()
	     (setf (slot-value patch 'deleted) t
		   (slot-value patch 'bknr.datastore::timestamp) (get-universal-time)))
	   (json-action-reply "delete-patch" "ok" nil)))))

(defun json-approve-patch (json)
  (let* ((patch-id (parse-integer (or (json-get-value :patch-id json) "") :junk-allowed t))
	 (patch (when patch-id
						(store-object-with-id patch-id)))
				 (author (when patch
									 (rw-patch-author patch)))
				 (user (json-get-user)))
		
    (format t "patch: ~A, user: ~A, author: ~A~%" patch user author)
		
    (cond ((null patch-id)
					 (format t "invalid")
					 (json-action-reply "approve-patch" "failed" "invalid patch id"))
					((null patch)
					 (format t "null~%")
					 (json-action-reply "approve-patch" "failed" "no patch with this id"))
					((not (or (and user (admin-p user))
										(json-check-auth author)))
					 (format t "auth ~%")
					 (json-action-reply "approve-patch" "failed"
															(format nil "no credentials to approve object ~A" patch)))
					(t
					 (approve-patch patch)
					 (json-action-reply "approve-patch" "ok" nil)))))

(defun json-delete-user (json)
  (let* ((user (json-get-user))
	 (delete-username (json-get-value :username json))
	 (delete-user (find-user delete-username)))
    (cond ((null user)
	   (json-action-reply "delete-user" "failed" (format nil "wrong username/password")))
	  ((not (admin-p user))
	   (json-action-reply "delete-user" "failed" "no credentials"))
	  ((null delete-user)
	   (json-action-reply "delete-user" "failed" (format nil "no user ~a found" delete-username)))
	  ((and user
		(admin-p user)
		(not (null delete-user)))
	   (delete-object delete-user)
	   (json-action-reply "delete-user" "ok" (format nil "user ~a deleted" delete-username)))
	  (t
	   (json-action-reply "delete-user" "failed" "unknown error")))))

(defun json-get-server-info (json)
  (json-action-reply "get-server-info" "ok" "server info"))

(defun json-get-client-info (json)
  (json-action-reply "get-client-info" "ok" nil `(("client-version" . ,*json-client-version*)
						  ("client-url" . ,*json-client-url*))))

(defun news-to-news-json (news)
  `(("title" . ,(bknr.text:article-subject news))
    ("news-id" . ,(store-object-id news))
    ("message" . ,(format nil "~A~A"
			  (rw-blog-article-short news)
			  (let ((text (bknr.text:article-text news)))
			    (if text
				(format nil "<br/>~A" text)
				""))))
    ("date" . ,(cybertiggyr-time:format-time nil "%Y-%m-%d" (bknr.text:article-time news)))))

(defun json-get-news (json)
  (let* ((date-since-str (json-get-value :date-since json))
	 (date-since (if date-since-str
			 (parse-time date-since-str)
			 0)))
  (json-action-reply "get-news" "ok" nil
		     `(("news-list" . ,(let ((res (loop for news in (news-posts :count nil :since date-since)
						     collect (news-to-news-json news))))
					    (if res
						res
						(make-array 0))))
		       ("date-since" . ,date-since-str)))))

(define-persistent-class bug-report ()
  ((bug-report :read)
   (created :read)
   (author :read)))

(defun json-send-bug-report (json)
  (let* ((bug-report-msg (json-get-value :bug-report json))
	 (bug-report (make-instance 'bug-report
                                    :author (json-username)
                                    :bug-report bug-report-msg
                                    :created (get-universal-time))))
    (cl-smtp:send-email "localhost" "info@ruinwesen.com" '("manuel@bl0rg.net" "patchmanager@fastmail.fm")
			(format nil "RW Bug Report from ~A" (json-username))
			bug-report-msg)
    (json-action-reply "send-bug-report" "ok" nil)))

(defun all-bug-reports ()
  (store-objects-with-class 'bug-report))

(defun json-get-auth (json)
  (json-get-value :auth json))

(defclass patch-manager-handler (page-handler)
	()
  )

(defmethod handle ((handler patch-manager-handler))
  (with-query-params ()
    (with-http-response ()
      (no-cache)
      (alexandria:when-let ((post-data (raw-post-data :request *request* :force-text t)))
        (patch-dispatch-json (yason:parse post-data))))))

(defun patch-dispatch-json (json)
  (let ((action (json-get-value :action json))
        (*json-auth* (json-get-auth json))
        (protocol-id (json-get-value :protocol-id json)))
    ;;    (format *standard-output* "~S~%" json)
    (alexandria:if-let ((handler (find-symbol (format nil "~A-~A" '#:json action))))
      (funcall handler json)
      (json-action-reply action "failed" (format nil "unknown action ~S" action)))))

(defclass patch-serve-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler patch-serve-handler))
  (let ((id-or-name (parse-url)))
    (when id-or-name
      (find-store-object id-or-name :class 'rw-patch :query-function #'store-object-with-id))))

(defmethod handle-object ((handler patch-serve-handler) patch)
  (with-http-response (:content-type "application/zip")
    (let ((stream (send-headers)))
      (blob-to-stream patch stream)
      (force-output stream))))
