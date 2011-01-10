(in-package :cl-user)

(asdf:defsystem :md
  :description "MachineDrum helper"
  :author "Manuel Odendahl"
  :serial t
  :components ((:file "package")
               (:file "config" :depends-on ("package"))
	       (:file "utils" :depends-on ("package" "config"))

	       (:file "md-params" :depends-on ("package" "config"))
	       (:file "store" :depends-on ("package" "config"))
	       (:file "montezuma" :depends-on ("package" "config"))
	       
	       (:file "md" :depends-on ("package" "utils" "md-params" "store" "montezuma"))

	       (:file "gd" :depends-on ("package" "utils" "md"))
               
	       (:file "midi" :depends-on ("package" "utils" "md-params" "md"))
	       (:file "sysex" :depends-on ("package" "utils" "md" "midi"))
	       (:file "elektron" :depends-on ("package" "sysex" "midi" "md" "utils"))

	       (:file "group" :depends-on ("package" "md" "utils"))
               (:file "md-utils" :depends-on ("package" "md" "group"))
	       (:file "export" :depends-on ("package" "md" "utils" "group"))
               (:file "web/md-handler" :depends ("package" "md" "md-utils" "gd"))

	       )
  :depends-on (:cm
	       :cl-gd
	       :cl-fad
	       :cl-who
	       :bknr.datastore
	       :cl-ppcre
	       :cl-interpol
	       :cl-json
	       :montezuma
               :closer-mop
	       ))
	       