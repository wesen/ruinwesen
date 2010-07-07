(defpackage :inventory.system
	(:use :asdf :cl))

(in-package :inventory.system)

(defsystem :inventory
	:name "INVENTORY"
	:author "Manuel Odendahl <wesen@ruinwesen.com>"
	:version "0.1"
	:maintainer "Manuel Odendahl <wesen@ruinwesen.com>"
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
	 (:file "inventory")
	 ))
	