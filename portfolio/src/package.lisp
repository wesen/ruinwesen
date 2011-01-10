(in-package :cl-user)

(defpackage :portfolio.config
	(:use :cl :cl-user)
	(:export
	 #:*website-url*
	 #:*website-directory*
         #:*xml-catalog-directory*
         #:*store-directory*
         #:*webserver-port*))

(defpackage :portfolio
	(:documentation "Package for the PORTFOLIO system")
  (:nicknames :template)
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
	:alexandria
        )
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:shadowing-import-from :alexandria #:array-index)
  )

(defpackage :portfolio.tags
  (:use :cl
        :cl-user
        :bknr.web
        :xhtml-generator
        :bknr.datastore
        :bknr.indices
        :bknr.images
        :bknr.user
        :hunchentoot
        :cl-interpol
        :cl-ppcre
        :bknr.utils
        :portfolio)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars))

(in-package :portfolio)