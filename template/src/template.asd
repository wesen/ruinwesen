(defpackage :template.system
	(:use :asdf :cl))

(in-package :template.system)

(defsystem :template
	:name "TEMPLATE"
	:author "template author <foo@foo.com>"
	:version "0.1"
	:maintainer "template author <foo@foo.com>"
	:serial t
	:depends-on (:cl-interpol
               :cl-ppcre
               :cxml
               :cl-mime
               :drakma
               :bknr.web
               :bknr.datastore
               :bknr.modules
               :cl-gd
               :unit-test
               :yason
               :cl-pdf)

  
	:components
	((:file "package")
	 (:file "config" :depends-on ("package"))
	 (:file "init" :depends-on ("config"))
	 (:file "template" :depends-on ("config"))
   (:file "tags" :depends-on ("template"))
	 ))