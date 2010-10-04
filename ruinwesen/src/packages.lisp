(in-package :cl-user)

(defpackage :ruinwesen.config
  (:use :cl :cl-user)
  (:export #:*website-url*
	   #:*website-directory*
	   #:*webserver-port*
	   #:*store-directory*
	   #:*thirdparty-directory*
	   #:*xml-catalog-directory*
	   ))

(defpackage :ruinwesen
  (:use :cl
	:cl-user
	:cl-interpol
	:cl-ppcre
	:cl-gd
	:cl-smtp
	:hunchentoot
	:bknr.utils
	:bknr.web
	:bknr.user
	:bknr.datastore
	:bknr.indices
	:bknr.images
	:bknr.rss
	:bknr.text
	:alexandria
	:cxml
	:ruinwesen.config
	:xhtml-generator)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:export
   #:product #:product-with-name #:product-name #:product-price #:currency #:amount
   #:product-pictures #:product-keywords #:product-soundfiles #:product-description
   #:soundfile #:soundfile-description
   
   #:ruinwesen-news #:ruinwesen-news-text #:ruinwesen-news-title #:ruinwesen-news-date
   #:startup))

(defpackage :ruinwesen.tags
  (:use :cl
	:cl-user
	:cl-interpol
	:bknr.web
	:xhtml-generator
	:bknr.utils
	:bknr.user
	:bknr.datastore
	:ruinwesen
	:ruinwesen.config)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  ; (:export )
  )