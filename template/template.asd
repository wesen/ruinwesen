(defpackage :template.system
	(:use :asdf :cl))

(in-package :template.system)

(defsystem :template
	:name "TEMPLATE"
	:author "template author <foo@foo.com>"
	:version "0.1"
	:maintainer "template author <foo@foo.com>"
	:serial t
	:depends-on (:unit-test
							 :cl-ppcre
							 :bknr.datastore
							 :bknr.modules
							 :bknr.web
							 :hunchentoot
							 :drakma
							 :html-match
							 :phtml)

	:components
	((:file "package")
	 (:file "config")
	 (:file "init")
	 (:file "template")
	 ))
	