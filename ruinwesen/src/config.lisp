(in-package :ruinwesen.config)

;; this is kind of a dirty hack to get the root of the thirdparty directory
(defparameter *thirdparty-directory*
  (asdf:system-relative-pathname :cl-gd #p"../"))

(defparameter *website-url* "http://jockel.hasis.biz:4242/")

(defparameter *dollars-to-eur* 1.5)

(defparameter *root-directory* (asdf:system-relative-pathname :ruinwesen #p"../"))
(defparameter *xml-catalog-directory* (asdf:system-relative-pathname :ruinwesen #p"../xml/"))

(defparameter *store-directory* (merge-pathnames #p"datastore/" *root-directory*))

(defparameter *website-directory* (probe-file (merge-pathnames #p"website/" *root-directory*)))

(defparameter *webserver-port* 4141)