(in-package :ruinwesen)

(deftransaction bknr.datastore::make-object (class &rest initargs)
  (apply #'make-instance class initargs))

(defun hunchentoot::script-name* ()
  (hunchentoot:script-name))

(defun hunchentoot::get-parameters* (&optional request)
  (hunchentoot::get-parameters))

(defun hunchentoot::post-parameters* ()
  (hunchentoot::post-parameters))

