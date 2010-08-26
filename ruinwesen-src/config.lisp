(in-package :ruinwesen.config)

(defparameter *website-url* "http://bknr.net:4242/")

(defparameter *dollars-to-eur* 1.5)

(defparameter *root-directory*
  (merge-pathnames #P"../" (make-pathname :directory (pathname-directory *load-pathname*))))

(defparameter *store-directory*
  (merge-pathnames #p"datastore/" *root-directory*))

(defparameter *website-directory*
  (probe-file (merge-pathnames #p"ruinwesen-website/" *root-directory*)))

(defparameter *webserver-port* 4242)