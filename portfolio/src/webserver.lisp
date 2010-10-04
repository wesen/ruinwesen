(in-package :portfolio)

(defun publish-portfolio ()
  (bknr.web:unpublish)
  (make-instance 'bknr.web:website
		 :name "Portfolio system"
		 :url "portfolio.ruinwesen.com"
		 :handler-definitions
		 `(
		   ;; administration handlers
		   user
		   images

		   ;; static file serving
		   ("/static" bknr.web:directory-handler
			      :destination ,(merge-pathnames #p"static/" *website-directory*)
			      :filename-separator #\,)       

		   ;; default template handler
		   ("/" bknr.web:template-handler
			:default-template "index"
			:catch-all t
			:destination ,(namestring (merge-pathnames "templates/" *website-directory*))
			:command-packages (("http://bknr.net" . :bknr.web)
					   ("http://portfolio.ruinwesen.com" . :portfolio.tags))))
		 
		 :authorizer (make-instance 'bknr.web:bknr-authorizer)
		 ))

