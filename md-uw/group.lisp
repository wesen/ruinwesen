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

(defun kit-rom-machines (kit)
  (remove-if-not #'rom-model-p (kit-machines kit) :key #'machine-model))

(defun kit-samples (kit)
  (remove-duplicates (mapcar #'machine-sample (kit-rom-machines kit))
		     :test #'string-equal))

(defun pattern-samples (bla)
  (remove-duplicates (mapcan #'kit-samples (collect-machines (list bla)))
		     :test #'string-equal))

(defun group-samples (bla)
  (sort (remove-duplicates (mapcan #'pattern-samples (group-patterns bla))
			   :test #'string-equal) #'string<))

(defun import-file (file samples)
  (remove-if #'md-object-empty-p
	     (read-elektron-file file :samples samples)))

(defmethod song-patterns ((song md-song))
  (let ((rows (md-song-rows song)))
    (remove :LOOP (remove :END (remove :JUMP (remove-duplicates
					      (mapcar #'md-row-pattern rows)))))))

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
  (make-object 'group :name name
	       :objects (import-file file samples))) 