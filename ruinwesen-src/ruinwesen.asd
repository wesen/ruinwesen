(in-package :cl-user)

(defpackage :ruinwesen.system
  (:use :cl :asdf))

(in-package :ruinwesen.system)

(defsystem :ruinwesen
  :name "ruinwesen"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :version "0"
  :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
  :licence "BSD"
  :description "ruinwesen electronics site"
  :long-description ""

  :depends-on (:cl-interpol
	       :cl-ppcre
	       :cxml
	       :cl-mime
	       :cl-smtp
	       :bknr.web
	       :bknr.datastore
	       :bknr.modules
	       :parse-number
	       :cybertiggyr-time
	       :cl-gd)

  :serial t

  :components ((:file "packages")
	       (:file "config")
	       (:file "money")
	       (:file "patch")
	       (:file "products")
	       (:file "news")
	       (:file "twitter")
	       (:file "atom")
	       (:file "blog")
	       (:file "handlers")
	       (:file "tags")
	       (:file "login")
	       (:file "webserver")
	       (:file "init")))
