(in-package :cl-user)

(defpackage :template
	(:documentation "Package for the TEMPLATE system")
	(:use :cl :cl-ppcre :bknr.datastore))

(defpackage :template.config
	(:use :cl :cl-user)
	(:export
	 #:*website-url*
	 #:*website-directory*))

(in-package :template)