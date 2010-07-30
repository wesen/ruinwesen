(defpackage :template.system
	(:use :asdf :cl))

(in-package :template.system)

(defsystem :template
	:name "TEMPLATE"
	:author "template author <foo@foo.com>"
	:version "0.1"
	:maintainer "template author <foo@foo.com>"
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
               :cl-pdf
               :cybertiggyr-time)

  
	:components
	((:file "package")
	 (:file "config" :depends-on ("package"))
	 (:file "init" :depends-on ("config"))
	 (:file "template" :depends-on ("config"))
   (:file "project" :depends-on ("template"))
   (:file "tags" :depends-on ("template" "project" "utils"))
   (:file "utils" :depends-on ("template" "project"))
	 ))
