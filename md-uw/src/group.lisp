(in-package :md)

(defun collect-machines (patterns-or-group)
  (let ((patterns (if (typep patterns-or-group 'group)
		      (group-patterns patterns-or-group)
		      patterns-or-group)))
    (remove-duplicates (mapcar #'md-pattern-kit patterns))))

(defun group-patterns (group)
  (remove-duplicates (append (remove-if-not #'(lambda (x) (typep x 'md-pattern))
					    (group-objects group))
			     (mapcan #'song-patterns (group-songs group)))))

(defun group-songs (group)
  (remove-if-not #'(lambda (x) (typep x 'md-song))
		 (group-objects group)))

(defun group-samples (bla)
  (sort (remove-duplicates (mapcan #'pattern-samples (group-patterns bla))
			   :test #'string-equal) #'string<))

(defmethod group-objects ((list list))
  list)

(defun group-kits (group)
  (let ((res))
    (loop for obj in (append (group-objects group)
			     (group-patterns group))
	 do (typecase obj
	      (kit (push obj res))
	      (md-pattern (push (md-pattern-kit obj) res)))
	 finally (return (remove-duplicates res)))))

(defun import-file-into-group (file name &optional samples)
  (make-instance 'group :name name
	       :objects (import-file file samples)))

(defun export-group (bla outputdir &key force (export-samples nil))
  (let (patterns kits songs)
    (loop for obj in (if (typep bla 'group)
			 (group-objects bla)
			 bla)
	 do (typecase obj
	      (md-pattern (pushnew obj patterns :test #'equal)
			  (pushnew (md-pattern-kit obj) kits :test #'equal))
	      (kit (pushnew obj kits))
	      (md-song (pushnew obj songs :test #'equal)
		       (loop for pat in (song-patterns obj)
			    do (pushnew pat patterns :test #'equal)
			    (pushnew (md-pattern-kit pat) kits :test #'equal)))))
    (let ((samples (sort (remove-duplicates (mapcan #'kit-samples kits))
			 #'string< :key #'pathname-name)))
      (when (directory-exists-p outputdir)
	(if force
	    (delete-directory-and-files outputdir)
	    (error "output directory already exists")))
      (ensure-directories-exist outputdir)
      (export-objects (append kits patterns songs)
		      (make-pathname :name "output" :type "syx"
				     :defaults outputdir))
      (when export-samples
	(let ((sample-names (mapcar #'(lambda (x)
					(if (typep bla 'group)
					    (format nil "~A ~A"
						    (pathname-name x)
						    (group-name bla))
					    (pathname-name x))) samples)))
	  (loop for sample in samples
	     for name in sample-names
	     do (format t "copying file ~A~%" sample)
	       (copy-file sample (make-pathname :name name
						:type (pathname-type sample)
						:defaults outputdir)))
	  (write-c6-file (make-pathname :name "output" :type "c6"
					:defaults outputdir)
			 (cons "output.syx" sample-names) :relative t)))
	
      (list samples ))))