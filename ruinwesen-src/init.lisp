(in-package :ruinwesen)

(defvar *server* nil)

(defun startup ()
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
    (hunchentoot:stop *server*))

  (setf *server* (hunchentoot:start (make-instance 'hunchentoot:acceptor
                                                   :port ruinwesen.config:*webserver-port*))))

(defmethod bknr.indices::destroy-object-with-class ((class t) (object t)))
