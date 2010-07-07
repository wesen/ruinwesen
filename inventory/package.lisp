(in-package :cl-user)

(defpackage :inventory
	(:documentation "Package for the INVENTORY system")
	(:use :cl :cl-ppcre :bknr.datastore))

(defpackage :inventory.config
	(:use :cl :cl-user)
	(:export
	 #:*website-url*
	 #:*website-directory*))

(in-package :inventory)