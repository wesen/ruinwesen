(in-package :ruinwesen)

(enable-interpol-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\$ #'(lambda (stream char char2)
					    (list '$ (read stream t nil t)))))

(defun publish-ruinwesen ()
  (setf bknr.web::*upload-file-size-limit* (* 30 1024 1024))
  (unpublish)
  (make-instance 'website
		 :name "Ruinwesen"
		 :handler-definitions
		 `(
		   ("/atom" atom-handler)
		   ("/submit-contact" contact-handler
				      :destination ,(namestring (merge-pathnames "templates/" *website-directory*))
				      :command-packages (("http://ruinwesen.com/" . :ruinwesen.tags)
							 ("http://bknr.net/" . :bknr.web)))
				      
		   ("/submit-preorder" preorder-handler
				      :destination ,(namestring (merge-pathnames "templates/" *website-directory*))
				      :command-packages (("http://ruinwesen.com/" . :ruinwesen.tags)
							 ("http://bknr.net/" . :bknr.web)))
				      

		   ("/rss" rss-handler)
		   ("/login" login-handler)
		   ("/admin" admin-handler)
		   ("/static" directory-handler
			      :destination ,(merge-pathnames #p"static/" *website-directory*))
		   ("/support-files" directory-handler
			      :destination ,(merge-pathnames #p"support/" *website-directory*))
		   ("/images" directory-handler
			      :destination ,(merge-pathnames #p"static/images/" *website-directory*))
		   user
		   ("/patch/" patch-serve-handler)
		   ("/get-patch" patch-serve-handler)
		   ("/patch-manager" patch-manager-handler)
		   ("/" template-handler
			:default-template "index"
			:destination ,(namestring (merge-pathnames "templates/" *website-directory*))
			:command-packages (("http://ruinwesen.com/" . :ruinwesen.tags)
					   ("http://bknr.net/" . :bknr.web)))
		   #+nil("/js/" parenscript-file-handler
			   :destination ,(namestring (merge-pathnames "parenscript/"
								      *website-directory*)))
		   #+nil user
		   #+nil images
		   ("/favicon.ico" file-handler
				   :destination ,(merge-pathnames #p"static/favicon.ico" *website-directory*)
				   :content-type "application/x-icon"))
		 :authorizer (make-instance 'bknr-authorizer)
		 :style-sheet-urls '("/static/styles.css")
		 ;; :javascript-urls '("/js/ruinwesen")
		 ))
