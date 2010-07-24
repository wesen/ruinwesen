(in-package :md)

(defun prototype-reader (stream char2)
  (let ((char (peek-char nil stream)))
    (if (not (eql char #\())
	(let ((char (read-char stream))
	      (second (read stream t nil)))
	  `(,(intern (format nil "$~A" char) (find-package :keyword))
	     ,@second))
	`(,(make-keyword-from-string (format nil "~A" char2)) ,@(read stream t nil)))))

(defvar *previous-readtables* nil)

(defun %enable-prototype-syntax ()
  (push *readtable*
        *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-macro-character #\$ 'prototype-reader)
  (values))

(defun %disable-prototype-syntax ()
  (if *previous-readtables*
    (setq *readtable* (pop *previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-prototype-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-prototype-syntax)))

(defmacro disable-prototype-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-prototype-syntax)))

(defmacro js-script (&rest body)
  `(with-html-output (*standard-output*)
     ((:script :type "text/javascript")
      (cl-who:fmt "~%// <![CDATA[~%")
      (cl-who:str (js:js ,@body))
      (cl-who:fmt "~%// ]]>~%"))))

(defun create-function-dispatcher (prefix functions)
  (lambda (request)
    (let* ((name (script-name request))
	   (mismatch (mismatch name prefix :test #'char=)))
      (and (or (null mismatch)
               (>= mismatch (length prefix)))
	   (let* ((func (string-upcase (subseq name (length prefix))))
		  (symbol (find func functions :test #'string-equal
				:key #'symbol-name)))
	     (when symbol
	       (symbol-function symbol)))))))

(defmacro with-parameters ((&rest params) &rest body)
  `(let (,@(loop for param in params
	      if (symbolp param)
	      collect `(,param (parameter ,(string-downcase (symbol-name param))))
	      if (consp param)
	      collect `(,(first param) (or (parameter ,(string-downcase
							(symbol-name (first param))))
					   ,(second param)))))
     ,@Body))

(defmacro defun-ajax (name (&rest args) &rest body)
  `(defun ,name ()
     (with-parameters ,args
       ,@body)))

(defun js-create (&rest args)
  (let ((res (make-hash-table)))
    (loop for (key val) on args by #'cddr
	 do (setf (gethash key res) val))
    res))

(js::defjsmacro dom-elt (id)
  `(document.get-element-by-id ,id))

(js::defjsmacro dom-html (id)
  `(slot-value (dom-elt ,id) 'inner-h-t-m-l))

(js::defjsmacro js-defun-ajax (name (&rest args) &key url on-success)
  `(defun ,name (&rest args)
     (new (-ajax.-request ,url (create :method "post"
				       :parameters (create ,@(loop for arg in args
								appending (list (make-keyword arg)
										arg)))
				       :on-success (lambda (transport)
						     ,on-success))))))

