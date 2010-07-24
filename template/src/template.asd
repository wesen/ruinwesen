(defpackage :template.system
	(:use :asdf :cl))

(in-package :template.system)

(defsystem :foobar
	:depends-on (:bla))