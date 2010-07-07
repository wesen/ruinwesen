(in-package :inventory)

(defun html-search-site (url pattern)
	(html-match:html-search (car (phtml:parse-html (drakma:http-request url)))
													pattern))