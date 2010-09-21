(defpackage :portfolio.system
	(:use :asdf :cl))

(in-package :portfolio.system)

(defsystem :portfolio
	:name "PORTFOLIO"
	:author "Manuel Odendahl <wesen@ruinwesen.com>"
	:version "0.1"
	:maintainer "Manuel Odendahl <wesen@ruinwesen.com>"
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
	 (:file "portfolio" :depends-on ("config"))
	 (:file "project" :depends-on ("portfolio"))
	 (:file "tags" :depends-on ("portfolio" "project" "utils"))
	 (:file "utils" :depends-on ("portfolio" "project"))
	 ))
