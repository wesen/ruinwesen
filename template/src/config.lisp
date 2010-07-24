(in-package :template)

(eval-when (:compile-toplevel)
	(defparameter *config-file-pathname* *compile-file-truename*))

(defparameter *website-url* "http://localhost:4242/")
(defparameter *root-directory* (merge-pathnames #p"../"
																								 (make-pathname :name nil :type nil
																																:version nil :defaults *config-file-pathname*)))
(defparameter *store-directory* (merge-pathnames #p"datastore/" *root-directory*))
(defparameter *website-directory* (probe-file (merge-pathnames #p"website/" *root-directory*)))
(defparameter *webserver-port* 4242)
