(in-package :cl-user)

(defpackage :template.config
	(:use :cl :cl-user)
	(:export
	 #:*website-url*
	 #:*website-directory*))

(defpackage :template
	(:documentation "Package for the TEMPLATE system")
	(:use :cl
        :cl-ppcre
        :bknr.datastore
        :cl-interpol
        :bknr.utils
        :bknr.web
        :hunchentoot
        :bknr.user
        :bknr.indices
        :bknr.images
        :bknr.rss
        )
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:shadowing-import-from :alexandria #:array-index)
  )

(defpackage :template.tags
  (:use :cl
        :cl-user
        :bknr.web
        :xhtml-generator
        :bknr.utils
        :template)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars))

(in-package :template)