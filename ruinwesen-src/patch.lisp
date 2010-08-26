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
   (name :update :initform "")
   (hex-data :update :initform nil)
   (documentation :update :initform nil)))

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

;; auto-unfold blob into slots

(defun patch-unfold-blob (patch)
  (let* ((temporary-directory (cl-fad:pathname-as-directory
                               (make-temporary-pathname :name "rw-blob" :defaults #P"/tmp/")))
         (*default-pathname-defaults* temporary-directory))
    (ensure-directories-exist temporary-directory)
    (asdf:run-shell-command "cd ~A && unzip ~A"
                            (namestring temporary-directory)
                            (namestring (blob-pathname patch)))
    (with-transaction (:unfold-blob)
      (with-slots (hex-data documentation) patch
        (setf hex-data
              (if (probe-file "mididata.hex")
                  (file-contents "mididata.hex" :element-type 'character)
                  "")
              documentation
              (if (probe-file "documentation.txt")
                  (file-contents "documentation.txt" :element-type 'character)
                  ""))))
    (cl-fad:delete-directory-and-files temporary-directory)))

(defmethod rw-patch-hex-data :before ((patch rw-patch))
  (unless (slot-value patch 'hex-data)
    (patch-unfold-blob patch)))

(defmethod rw-patch-documentation :before ((patch rw-patch))
  (unless (slot-value patch 'documentation)
    (patch-unfold-blob patch)))

#+nil
(make-instance 'rw-patch
               :author (find-user "wesen")
               :title "MDWesenLivePatch"
               :comment "my live patch"
               :tags '(:machinedrum :live :delay :supatrigga :midiclock)
               :device-id :minicommand2
               :name "MDWesenLivePatch")

(defparameter *json-protocol-id* "1.1")
(defparameter *json-client-version* "1.0")
(defparameter *json-client-url* "http://ruinwesen.com/minicommand")

(defmacro with-json-action-reply ((action status &optional format &rest args)
                                  &body body)
  `(yason:with-output-to-string* ()
     (yason:with-object ()
       (yason:encode-object-elements
        "protocol-id" *json-protocol-id*
        "action" "response"
        "requested-action" ,action
        "status" ,status
        ,@(when format
                `("message" (format nil ,format ,@args))))
       ,@body)))

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

(defpackage :json-actions)

(defmacro define-json-action (name args &body body)
  `(defun ,(intern (string name) :json-actions) ,args ,@body))

(define-json-action json-register-new-user (json)
  (declare (ignore json))
  (let ((user (json-get-value :username *json-auth*))
	(password (json-get-value :password *json-auth*))
	(email (json-get-value :email *json-auth*)))
    (if (or (null user)
	    (null password)
	    (string= user ""))
	(with-json-action-reply ("register-new-user" "failed" "invalid user information"))
	(handler-case
	    (progn
              (make-user user :password password :email email)
              (with-json-action-reply ("register-new-user" "ok"  "registered new user ~S" user)))
	  (error (e)
	    (declare (ignore e))
	    (with-json-action-reply ("register-new-user" "failed" "login ~S already exists" user)))))))

(defun json-get-value (name json)
  (gethash (string-downcase (string name)) json))

(defun json-get-metadata-value (name json)
  (let ((metadata (json-get-value :patch-metadata json)))
    (when metadata
      (json-get-value name metadata))))

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

(defun patch-to-json (patch &key full)
  (yason:with-object ()
    (with-slots (title comment author tags deleted last-modified device-id name) patch
      (yason:encode-object-elements
       "title" title
       "id" (store-object-id patch)
       "comment" comment
       "author" author
       "tags" (mapcar #'string-downcase tags)
       "last-modified-date" (cybertiggyr-time:format-time nil "%Y-%m-%d" last-modified)
       "device-id" device-id
       "name" name)
      (when full
        (yason:encode-object-elements
         "documentation" (rw-patch-documentation patch)
         "hex-data" (rw-patch-hex-data patch))))))

(defun patch-to-patch-source-json (patch)
  (yason:with-object ()
    (with-slots (deleted) patch
      (yason:encode-object-elements
       "patch-id" (store-object-id patch)
       "patch-url" (format nil "/patch/~A" (store-object-id patch)))
      (when deleted
        (yason:encode-object-element "deleted" deleted))
      (yason:with-object-element ("patch-metadata")
        (patch-to-json patch)))))
 
(define-json-action json-store-new-patch (json)
  (if (json-check-auth)
      (let ((patch (make-patch-from-json json)))
	(if (null patch)
	    (with-json-action-reply ("store-new-patch" "failed" "could not create/store/decode patch"))
	    (with-json-action-reply ("store-new-patch" "ok")
              (yason:with-object-element ("patch-source")
                (patch-to-patch-source-json patch)))))
      (with-json-action-reply ("store-new-patch" "failed" "username/password incorrect"))))

(defun find-patches-since (&key (since 0) (approved t))
  (sort (remove-if-not #'(lambda (x)
			   (and (>= (blob-timestamp x) since)
				(if approved
				    (not (member :needs-to-be-approved (rw-patch-tags x)))
				    t)))
		       (store-objects-with-class 'rw-patch)) #'< :key #'blob-timestamp))

(define-json-action json-patch-source-list (json)
  (let* ((date-since-str (json-get-value :date-since json))
	 (date-since (if date-since-str
			 (parse-time date-since-str)
			 0))
	 (user (json-get-user)))
    (with-json-action-reply ("get-patch-source-list" "ok")
      (yason:with-object-element ("patch-source-list")
        (yason:with-array ()
          (dolist (patch (find-patches-since :since date-since
                                             :approved (or (not user)
                                                           (not (admin-p user)))))
            (patch-to-patch-source-json patch))))
      (yason:encode-object-element "date-since" date-since-str))))

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

(define-json-action json-delete-patch (json)
  (let* ((patch-id (parse-integer (or (json-get-value :patch-id json) "") :junk-allowed t))
	 (patch (when patch-id
		  (store-object-with-id patch-id)))
	 (author (when patch
		   (rw-patch-author patch)))
	 (user (json-get-user)))

    (format t "patch: ~A, user: ~A, author: ~A~%" patch user author)

    (cond ((null patch-id)
	   (format t "invalid")
	   (with-json-action-reply ("delete-patch" "failed" "invalid patch id")))
	  ((null patch)
	   (format t "null~%")
	   (with-json-action-reply ("delete-patch" "failed" "no patch with this id")))
	  ((not (or (and user (admin-p user))
		    (json-check-auth author)))
	   (format t "auth ~%")
	   (with-json-action-reply ("delete-patch" "failed" "no credentials to delete object ~A" patch)))
	  (t
	   (format t "foobar author ~A~%" author)
;;	   (delete-object patch)
	   (with-transaction ()
	     (setf (slot-value patch 'deleted) t
		   (slot-value patch 'bknr.datastore::timestamp) (get-universal-time)))
	   (with-json-action-reply ("delete-patch" "ok"))))))

(define-json-action json-approve-patch (json)
  (let* ((patch-id (parse-integer (or (json-get-value :patch-id json) "") :junk-allowed t))
	 (patch (when patch-id
						(store-object-with-id patch-id)))
				 (author (when patch
									 (rw-patch-author patch)))
				 (user (json-get-user)))
		
    (format t "patch: ~A, user: ~A, author: ~A~%" patch user author)
		
    (cond ((null patch-id)
           (format t "invalid")
           (with-json-action-reply ("approve-patch" "failed" "invalid patch id")))
          ((null patch)
           (format t "null~%")
           (with-json-action-reply ("approve-patch" "failed" "no patch with this id")))
          ((not (or (and user (admin-p user))
                    (json-check-auth author)))
           (format t "auth ~%")
           (with-json-action-reply ("approve-patch" "failed"
                                                    "no credentials to approve object ~A" patch)))
          (t
           (approve-patch patch)
           (with-json-action-reply ("approve-patch" "ok"))))))

(define-json-action json-delete-user (json)
  (let* ((user (json-get-user))
	 (delete-username (json-get-value :username json))
	 (delete-user (find-user delete-username)))
    (cond ((null user)
	   (with-json-action-reply ("delete-user" "failed" "wrong username/password")))
	  ((not (admin-p user))
	   (with-json-action-reply ("delete-user" "failed" "no credentials")))
	  ((null delete-user)
	   (with-json-action-reply ("delete-user" "failed" "no user ~a found" delete-username)))
	  ((and user
		(admin-p user)
		(not (null delete-user)))
	   (delete-object delete-user)
	   (with-json-action-reply ("delete-user" "ok" "user ~a deleted" delete-username)))
	  (t
	   (with-json-action-reply ("delete-user" "failed" "unknown error"))))))

(define-json-action json-get-server-info (json)
  (declare (ignore json))
  (with-json-action-reply ("get-server-info" "ok" "server info")))

(define-json-action json-get-client-info (json)
  (declare (ignore json))
  (with-json-action-reply ("get-client-info" "ok")
    (yason:encode-object-elements
     "client-version" *json-client-version*
     "client-url" *json-client-url*)))

(defun news-to-news-json (news)
  (yason:with-object ()
    (yason:encode-object-elements
     "title" (bknr.text:article-subject news)
     "news-id" (store-object-id news)
     "message" (format nil "~A~@[<br/>~A~]"
                       (rw-blog-article-short news)
                       (bknr.text:article-text news))
     "date" (cybertiggyr-time:format-time nil "%Y-%m-%d" (bknr.text:article-time news)))))

(define-json-action json-get-news (json)
  (let* ((date-since-str (json-get-value :date-since json))
	 (date-since (if date-since-str
			 (parse-time date-since-str)
			 0)))
    (with-json-action-reply ("get-news" "ok")
      (yason:with-object-element ("news-list")
        (yason:with-array ()
          (dolist (news (news-posts :count nil :since date-since))
            (news-to-news-json news))))
      (yason:encode-object-element "date-since" date-since-str))))

(define-persistent-class bug-report ()
  ((bug-report :read)
   (created :read)
   (author :read)))

(define-json-action json-send-bug-report (json)
  (let* ((bug-report-msg (json-get-value :bug-report json)))
    (make-instance 'bug-report
                   :author (json-username)
                   :bug-report bug-report-msg
                   :created (get-universal-time))
    (cl-smtp:send-email "localhost" "info@ruinwesen.com" '("manuel@bl0rg.net" "patchmanager@fastmail.fm")
			(format nil "RW Bug Report from ~A" (json-username))
			bug-report-msg)
    (with-json-action-reply ("send-bug-report" "ok"))))

(defun all-bug-reports ()
  (store-objects-with-class 'bug-report))

(defun json-get-auth (json)
  (json-get-value :auth json))

(defclass patch-manager-handler (page-handler)
  ())

(defmethod handle ((handler patch-manager-handler))
  (with-http-response ()
    (no-cache)
    (alexandria:when-let ((post-data (raw-post-data :request *request* :force-text t)))
      (patch-dispatch-json (yason:parse post-data)))))

(defun patch-dispatch-json (json)
  (let* ((action (json-get-value :action json))
         (*json-auth* (json-get-auth json))
         #+(or) (protocol-id (json-get-value :protocol-id json))
         (handler (find-symbol (format nil "~:@(json-~A~)" action) :json-actions)))
    #+(or)
    (format *standard-output* "json: ~A action: ~A handler: ~A~%"
            (alexandria:hash-table-plist json) action handler)
    (if handler
        (funcall handler json)
        (with-json-action-reply (action "failed" "unknown action ~S" action)))))

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

(defclass patch-rest-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler patch-rest-handler))
  (alexandria:when-let ((id-or-name (parse-url)))
    (find-store-object id-or-name :class 'rw-patch :query-function #'store-object-with-id)))

(defmethod handle-object ((handler patch-rest-handler) patch)
  (with-http-response (:content-type (if (string-equal "application/json" (hunchentoot:header-in* :accept))
                                         "application/json"
                                         "text/plain"))
    (no-cache)
    (yason:with-output-to-string* ()
      (if patch
          (patch-to-json patch :full t)
          (if (parse-url)
              (error-404)
              (yason:with-object ()
                (yason:with-object-element ("patches")
                  (yason:with-array ()
                    (dolist (patch (find-patches-since))
                      (patch-to-json patch))))))))))
