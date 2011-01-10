(in-package :md)

(defvar *md-store* nil)

(defun start-store ()
  (setf *md-store*
	(make-instance 'store :directory *store-directory*
		       :subsystems (list (make-instance 'store-object-subsystem)
					 (make-instance 'blob-subsystem)))))

