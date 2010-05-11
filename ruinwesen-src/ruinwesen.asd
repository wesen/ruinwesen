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

  :components ((:file "packages")
	       (:file "config" :depends-on ("packages"))
	       (:file "money" :depends-on ("config"))
	       (:file "patch" :depends-on ("config"))
	       (:file "products" :depends-on ("config" "money"))
	       (:file "news" :depends-on ("config"))
	       (:file "twitter" :depends-on ("config"))
	       (:file "atom" :depends-on ("config"))
	       (:file "blog" :depends-on ("config"))
	       (:file "handlers" :depends-on ("config" "products" "news"))
	       (:file "tags" :depends-on ("products" "handlers"))
	       (:file "login" :depends-on ("products" "handlers"))
	       (:file "webserver" :depends-on ("config" "handlers" "tags" "login"))
	       (:file "init" :depends-on ("webserver"))))
