(in-package :ruinwesen)

(defvar *server* nil)

(defun startup (&key foregroundp (port *webserver-port*))
  (setf *hunchentoot-default-external-format*
        (flex:make-external-format :utf-8 :eol-style :lf))
  (close-store)
  (make-instance 'mp-store
                 :directory *store-directory*
                 :subsystems (list (make-instance 'store-object-subsystem)
                                   (make-instance 'blob-subsystem
                                                  :n-blobs-per-directory 1000)))

  (gather-twitters)
  (unless (class-instances 'bknr.cron::cron-job)
    (bknr.cron:make-cron-job "daily statistics"
                             'make-yesterdays-statistics 1 0 :every :every)
    (bknr.cron:make-cron-job "snapshot"
                             'snapshot-store 0 5 :every :every)
    (bknr.cron:make-cron-job "twitter update" 'gather-twitters))

  (cl-gd::load-gd-glue)
  (publish-ruinwesen)
  (bknr.cron:start-cron)

  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil))

  (flet
      ((start-fn ()
         (hunchentoot:start (make-instance 'hunchentoot:acceptor
                                           :port port
                                           :taskmaster (if foregroundp
                                                           (make-instance 'hunchentoot:single-threaded-taskmaster)
                                                           (make-instance 'hunchentoot:one-thread-per-connection-taskmaster))
                                           :request-dispatcher 'bknr.web:bknr-dispatch
                                           :persistent-connections-p t))))
    (if foregroundp
        (funcall #'start-fn)
        (bt:make-thread #'start-fn
                        :name (format nil "HTTP server on port ~A" port)))))

(defmethod bknr.indices::destroy-object-with-class ((class t) (object t)))
