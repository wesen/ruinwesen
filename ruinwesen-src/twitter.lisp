(in-package :ruinwesen)

(defparameter *last-twitters* nil)
(defparameter *last-twitters-ruin* nil)

(defun gather-twitters ()
  (let ((twitter (get-twitter "wesen" :count 3)))
    (when twitter
      (setf *last-twitters* twitter)))
  (let ((twitter (get-twitter "RuinMusic" :count 3)))
    (when twitter
      (setf *last-twitters-ruin* twitter))))

(defun get-text-body (node)
  (unless (= (length (dom:child-nodes node)) 0)
    (dom:node-value (aref (dom:child-nodes node) 0)  )))

(defun get-twitter (user &key (count 5))
  (let (res
	(doc (cxml:parse-octets
	      (drakma:http-request (format nil
					   "http://twitter.com/statuses/user_timeline/~A.xml?count=~A" user count))
	      (cxml-dom:make-dom-builder))))
    (dom:do-node-list (node (dom:get-elements-by-tag-name doc "status"))
      (let ((text (aref (dom:get-elements-by-tag-name node "text") 0))
	    (id (aref (dom:get-elements-by-tag-name node "id") 0))
	    (date (aref (dom:get-elements-by-tag-name node "created_at") 0)))
	(push (list (twitter-date-to-string (get-text-body date))
		    (get-text-body text)
		    (get-text-body id)) res)))
    (nreverse res)))

(defvar *months*
  '(("Jan" . 1)
    ("Feb" . 2)
    ("Mar" . 3)
    ("Apr" . 4)
    ("May" . 5)
    ("Jun" . 6)
    ("Jul" . 7)
    ("Aug" . 8)
    ("Sep" . 9)
    ("Oct" . 10)
    ("Nov" . 11)
    ("Dec" . 12)))

(defun twitter-date-to-string (date)
  (twitter-difference (twitter-date date)))

(defun twitter-date (date)
  (let* ((elts (cl-ppcre:split "\\s+" date))
	 (month (cdr (assoc (second elts) *months* :test #'string-equal)))
	 (day (parse-integer (third elts)))
	 (year (parse-integer (sixth elts)))
	 (time (fourth elts)))
    (+ 3600 (parse-time (format nil "~A/~A/~A ~A" month day year time)))))

(defun twitter-difference (date)
  (let ((diff (- (get-universal-time) date)))
    (twitter-diff-to-string diff)))

(defun twitter-diff-to-string (diff) 
  (cond
    ((< diff 5)
     "less than 5 seconds ago")
    ((< diff 10)
     "less than 10 seconds ago")
    ((< diff 30)
     "less than 30 seconds ago")
    ((< diff 60)
     "less than one minute ago")
    ((< diff (* 45 60))
     (format nil "~A minutes ago" (truncate diff 60)))
    ((< diff (+ 3600 (* 45 60)))
     (format nil "about one hour ago"))
    ((< (+ diff (* 15 60)) (* 24 3600))
     (format nil "about ~A hours ago" (truncate (+ diff (* 15 60)) 3600)))
    ((< diff (* 7 24 3600))
     (format nil "~A days ago" (truncate (+ diff (* 15 60)) (* 24 3600))))
    ((< diff (* 2 7 24 3600))
     "one week ago")
    (t
     (format nil "~A weeks ago" (truncate diff (* 7 24 3600))))))
