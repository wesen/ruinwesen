(in-package :portfolio)

(defvar *acceptor* nil
  "Stores the HUNCHENTOOT acceptor used to serve the website.")
(defvar *ht-thread* nil
  "Stores the thread used to run HUNCHENTOOT.")
(defvar *cron-actor* nil
  "Stores the CRON-ACTOR handling the cron jobs inside the datastore.")

(defun store-startup ()
  (close-store)
  (make-instance 'mp-store
		 :directory *store-directory*
		 :subsystems (list (make-instance 'store-object-subsystem)
				   (make-instance 'blob-subsystem
						  :n-blobs-per-directory 1000)))
  (unless (class-instances 'bknr.cron::cron-job)
    (bknr.cron:make-cron-job "snapshot" 'snapshot-store 0 5 :every :every))
  (bknr.utils:actor-start (setf *cron-actor* (make-instance 'bknr.cron::cron-actor))))

(defun startup (&key foregroundp (port *webserver-port*))
  (setq cxml::*default-catalog*
	(list (namestring (merge-pathnames #p"catalog" *xml-catalog-directory*))))
  (unless (probe-file (first cxml::*default-catalog*))
    (error "Could not find XML catalog.~%
Please make sure that ~A points to the correct location, or create
it if it is missing."
	   (first cxml::*default-catalog*)))
  (setf cxml::*catalog* (cxml:make-catalog))
  
  (setf *hunchentoot-default-external-format* (flex:make-external-format :utf-8 :eol-style :lf))

  (ensure-directories-exist
   (setf tbnl:*tmp-directory* (merge-pathnames "hunchentoot-tmp/" *store-directory*)))

  (when (probe-file "site-config.lisp")
    (format t "; loading site configuration file~%")
    (load "site-config.lisp"))

  (store-startup)
  (publish-portfolio)

  (start-http-server))

(defun shutdown-portfolio ()
  (actor-stop *cron-actor*)
  (stop-http-server)
  (close-store))

(defun stop-http-server ()
  "Stop the running webserver, and destroy the thread it was running in."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (bt:destroy-thread *ht-thread*)
    (setf *acceptor* nil)))

(defun start-http-server ()
  "Restart the webserver, republishing the website as well."
  (stop-http-server)

  (publish-portfolio)

  (setf *acceptor*
	(make-instance 'hunchentoot:acceptor
		       :port *webserver-port*
		       :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)
		       :persistent-connections-p nil
		       :request-dispatcher 'bknr.web:bknr-dispatch))

  (setf *ht-thread*
	(bt:make-thread (curry #'hunchentoot:start *acceptor*)
			:name (format nil "HTTP server on port ~A" *webserver-port*))))

(defmethod bknr.indices::destroy-object-with-class ((class t) (object t)))
