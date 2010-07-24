(in-package :template)

(defun store-startup ()
	(close-store)
	(make-instance 'mp-store
								 :directory *store-directory*
								 :subsystems (list (make-instance 'store-object-subsystem)
																	 (make-instance 'blob-subsystem
																									:n-blobs-per-directory 1000)))
	(unless (class-instances 'bknr.cron::cron-job)
		(bknr.cron:make-cron-job "snapshot" 'snapshot-store 0 5 :every :every))
	
	(bknr.utils:actor-start (make-instance 'bknr.cron::cron-actor)))

(defun publish-template ()
	(bknr.web:unpublish)
	(make-instance 'bknr.web:website
		 :name "Template system"
		 :handler-definitions
     `(
       ;; administration handlers
;;       ("/admin" bknr.web:admin-handler)
;;       ("/user"  bknr.web:user-handler)

       ;; static file serving
       ("/static" bknr.web:directory-handler
                  :destination ,(merge-pathnames #p"static/" *website-directory*)
                  :filename-separator #\,)       

       ;; default template handler
       ("/" bknr.web:template-handler
            :default-template "index"
            :catch-all t
            :destionation ,(namestring (merge-pathnames "templates/" *website-directory*))
            :command-packages (("http://bknr.net" . :bknr.web)
                               ("http://ruinwesen.com" . :template.tags))))
       
		 :authorizer (make-instance 'bknr.web:bknr-authorizer)
		 ))

(defun startup (&key foregroundp (port *webserver-port*))
	(setf hunchentoot:*hunchentoot-default-external-format*
				(flex:make-external-format :utf-8 :eol-style :lf))
	(ensure-directories-exist
	 (setf tbnl:*tmp-directory* (merge-pathnames "hunchentoot-tmp/" *store-directory*)))

	(store-startup)
	(publish-template)
	(when (probe-file "site-config.lisp")
		(format t "; loading site configuration file~%")
		(load "site-config.lisp"))

	(flet ((start-fn ()
					 (hunchentoot:start (make-instance 'hunchentoot:acceptor
																						 :port port
																						 :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)
																						 :request-dispatcher 'bknr.web:bknr-dispatch
																						 :persistent-connections-p nil))))
		(if foregroundp
				(funcall #'start-fn)
				(bt:make-thread #'start-fn
												:name (format nil "HTTP server on port ~A" port)))))

(defmethod bknr.indices::destroy-object-with-class ((class t) (object t)))
