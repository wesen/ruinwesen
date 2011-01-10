(in-package :portfolio)

(eval-when (:compile-toplevel :Execute)
  
	(defparameter *config-file-pathname* (asdf:system-relative-pathname :portfolio #p"config.lisp"))
  (defparameter bknr.web::*template-dtd-catalog* (list "/home/manuel/code/lisp/bknr-svn/thirdparty/xhtml/catalog.xml"))

  (setf cxml:*catalog* (cxml:make-catalog *template-dtd-catalog*)
	cxml:*dtd-cache* (cxml:make-dtd-cache)
	cxml:*cache-all-dtds* t)

  )
  


(defparameter *website-url* "http://localhost:4242/")
(defparameter *root-directory* (asdf:system-relative-pathname :portfolio #p"../"))
(defparameter *store-directory* (merge-pathnames #p"datastore/" *root-directory*))
(defparameter *website-directory* (probe-file (merge-pathnames #p"website/" *root-directory*)))
(defparameter *webserver-port* 4242)
