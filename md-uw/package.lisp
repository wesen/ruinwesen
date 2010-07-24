(in-package :cl-user)

(defpackage :md
  (:use :cl
	:cl-user
	:cm
	:cl-gd
	:bknr.utils
	:hunchentoot
	:cl-fad
	:cl-who
	:cl-ppcre
	:cl-interpol
	:bknr.datastore
	:bknr.indices
	:json)
  (:shadowing-import-from :cl-interpol :quote-meta-chars)
  (:shadowing-import-from :bknr.utils :copy-stream :copy-file))

(in-package :md)
(cl-interpol:enable-interpol-syntax)