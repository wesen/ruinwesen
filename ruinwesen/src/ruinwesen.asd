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
	       :cl-json
	       :cl-gd
	       :alexandria)

  :components ((:file "packages")

	       (:file "config" :depends-on ("packages"))

	       (:file "patch" :depends-on ("config"))
	       (:file "products" :depends-on ("config"))
	       (:file "news" :depends-on ("config"))
	       (:file "twitter" :depends-on ("config"))
	       (:file "blog" :depends-on ("config"))

               ;; atom handlers for editing the blog
	       (:file "atom" :depends-on ("config"))
               
               (:file "handlers" :depends-on ("config" "products" "news"))

	       (:file "tags" :depends-on ("products" "handlers"))
	       (:file "login" :depends-on ("products" "handlers"))

               (:file "backbone")

               (:file "webserver" :depends-on ("config" "handlers" "tags" "login" "backbone"))

               (:file "init" :depends-on ("webserver"))))
