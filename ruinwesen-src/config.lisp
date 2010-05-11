(in-package :ruinwesen.config)

(defparameter *website-url* "http://jockel.hasis.biz:4242/")

(defparameter *dollars-to-eur* 1.5)

(format t "pathname ~A~%" *load-pathname*)
(defparameter *root-directory*
  "/home/mnl/siff-svn/ruinwesen/")

(defparameter *store-directory*
  (merge-pathnames #p"datastore/" *root-directory*))

(defparameter *website-directory*
  (probe-file (merge-pathnames #p"website/" *root-directory*)))

(defparameter *webserver-port* 4242)