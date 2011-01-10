(in-package :cl-user)

(asdf:defsystem :md
  :description "MachineDrum helper"
  :author "Manuel Odendahl"
  :serial t
  :components ((:file "package")
	       (:file "utils" :depends-on ("package"))
	       (:file "md-params" :depends-on ("package"))
	       (:file "store" :depends-on ("package"))
	       (:file "montezuma" :depends-on ("package"))
	       
	       (:file "md" :depends-on ("package" "utils" "md-params" "store" "montezuma"))

	       (:file "midi" :depends-on ("package" "utils" "md-params" "md"))
	       (:file "sysex" :depends-on ("package" "utils" "md" "midi"))
	       (:file "elektron" :depends-on ("package" "sysex" "midi" "md" "utils"))
	       (:file "md-convert" :depends-on ("package" "midi" "md" "utils"))

	       (:file "gd" :depends-on ("package" "utils" "md"))
	       (:file "group" :depends-on ("package" "md" "utils"))
	       (:file "export" :depends-on ("package" "md" "utils" "group"))

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
	       