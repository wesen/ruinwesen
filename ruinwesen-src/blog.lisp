(in-package :ruinwesen)

(enable-interpol-syntax)

(define-persistent-class rw-blog-article (blog-article)
  ((short :update :initform nil)))

(defmethod rss-item-description ((article rw-blog-article))
  (format nil "~A~%~A" (rw-blog-article-short article)
	  (if (article-text article)
	      (article-text article)
	      "")))
							 

(defmethod rss-item-link ((article blog-article))
  (let ((channel (rss-item-channel article)))
    (if (equal (blog-name channel) "ruinwesen-news")
	(format nil "http://ruinwesen.com/index?start=~A" (store-object-id article))
	(format nil "http://ruinwesen.com/blog?id=~A" (store-object-id article)))))

(defmethod bknr.rss:rss-channel-link ((channel blog))
  (bknr.rss::rss-channel-path channel))

(defun post-to-blog (blog subject short text)
  (let ((wesen (find-user "wesen")))
    (with-transaction ()
      (let ((article (make-instance 'rw-blog-article
				  :short short
				  :author wesen
				  :subject subject
				  :text text)))
	(bknr.text::blog-add-article blog article)))))

(deftransaction blog-remove-article (blog article)
  (setf (blog-articles blog)
	(remove article (blog-articles blog)))
  (unless (object-destroyed-p article)
    (destroy-object article)))

(deftransaction blog-delete-article (article)
  (blog-remove-article (slot-value article 'blog) article))

(defun text-to-html2 (string)
  "Perform simple text to HTML conversion.  http urls are replaced by links, internal links to
images become image tags."
  (setf string (regex-replace-all
                #?r"bknr:([0-9A-Za-z$-_.+!*'()]+)" string
                #'(lambda (target-string start end match-start match-end reg-starts reg-ends)
                    (declare (ignore start end match-start match-end))
                    (let ((url (subseq target-string (aref reg-starts 0) (aref reg-ends 0))))
                      (regex-replace-all "URL" (if (all-matches "^/image" url)
                                                   "<img src=\"URL\" />"
                                                   "<a href=\"URL\">URL</a>")
                                         url)))))
  (setf string (regex-replace-all
                #?r" (http://[0-9A-Za-z$-_.+!*#'()~]+)" string
                #'(lambda (target-string start end match-start match-end &rest args)
                    (declare (ignore start end args))
                    (let ((url (subseq target-string match-start match-end)))
                      (regex-replace-all "URL" (if (all-matches "(?i)\\.(gif|jpe?g|png)$" url)
                                                   "<img src=\"URL\" />"
                                                   "<a href=\"URL\" target=\"_blank\">URL</a>")
                                         url)))))
  string)

(defun text-to-paragraphs (text)
  (scan-to-strings #?r"(.*(\r\n|\r|\n))([ \\t]*$)+" text))

(defun post-to-rw-blog (subject short text)
  (post-to-blog (bknr.text:blog-with-name "ruinwesen-blog") subject short text))

(defun post-to-rw-news (subject short text)
  (post-to-blog (bknr.text:blog-with-name "ruinwesen-news") subject short text))

(defun blog-posts (blog &key (count 5) (start 0) (since nil))
  (let* ((items (remove-if-not #'(lambda (x) (if since
						 (>= (bknr.text:article-time x) since)
						 t))
			       (sort (copy-tree (bknr.text:blog-articles blog))
				     #'> :key #'bknr.text:article-time)))
         (nitems (length items)))
    (when (< start nitems)
      (subseq items start (min (+ start (or count nitems)) (length items))))))

(defun rw-posts (&key (count 5) (start 0))
  (blog-posts (bknr.text:blog-with-name "ruinwesen-blog") :count count :start start))

(defun news-posts (&key (count 5) (start 0) (since nil))
  (blog-posts (bknr.text:blog-with-name "ruinwesen-news") :count count :start start :since since))
