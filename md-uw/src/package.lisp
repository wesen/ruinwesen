(in-package :cl-user)

(defpackage :md
  (:use :cl
	:cl-user
	:cm
	:cl-gd
	:bknr.utils
	:cl-fad
	:cl-who
	:cl-ppcre
	:cl-interpol
	:bknr.datastore
	:bknr.indices
        )
  (:shadowing-import-from :cl-interpol :quote-meta-chars)
  (:shadowing-import-from :bknr.utils :copy-file))

(in-package :md)
(cl-interpol:enable-interpol-syntax)
