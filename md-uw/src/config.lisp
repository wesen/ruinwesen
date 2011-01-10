(in-package :md)

(defparameter *root-directory* (asdf:system-relative-pathname :md #p"../"))
(defparameter *store-directory* (merge-pathnames #p"datastore/" *root-directory*))
(defparameter *image-directory* (merge-pathnames #p"pngs/" *root-directory*))
(defparameter *image-cache-directory* (merge-pathnames #p"image-cache/" *root-directory*))
(ensure-directories-exist *image-cache-directory*)