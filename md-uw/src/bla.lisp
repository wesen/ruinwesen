(in-package :md)

(defun get-pattern-by-name (name patterns)
  (find name patterns :key #'(lambda (x) (pattern-name (md-pattern-position x)))
	:test #'equal))

(defun get-patterns (names patterns)
  (remove-duplicates (mapcar #'(lambda (x) (get-pattern-by-name x patterns)) names)))

(defun show-kit (kit)
  (format t "~%~A~%" (kit-name kit))
  (loop for machine in (sort (copy-tree (kit-machines kit)) #'< :key #'machine-index)
     do (format t "machine ~A: ~A~%" (machine-index machine) (machine-name machine))))

(defun show-pats (patterns)
  (dolist (pat patterns)
    (format t "pat: ~A, kit: ~A~%" (pattern-name (md-pattern-position pat)) (kit-name (md-pattern-kit pat)))
    (show-kit (md-pattern-kit pat))))
(defun patterns-with-kit (kit patterns)
  (remove-if-not #'(lambda (x) (equal kit (md-pattern-kit x))) patterns))

(defun reorder-kit (kit reorder)
  (let ((oldpos (first reorder))
	(newpos (second reorder))
	(machines (kit-machines kit))
	(lfos (kit-lfos kit)))
    (change-slot-values (elt machines oldpos) 'index newpos)
    (change-slot-values (elt machines newpos) 'index oldpos)
    (change-slot-values (elt lfos oldpos) 'index newpos)
    (change-slot-values (elt lfos newpos) 'index oldpos)))

(defun reorder-pattern (pattern reorder)
  (let ((oldpos (first reorder))
	(newpos (second reorder))
	(tracks (md-pattern-tracks pattern)))
    (change-slot-values (elt tracks oldpos) 'index newpos)
    (change-slot-values (elt tracks newpos) 'index oldpos)))

(defun reorder-kit-patterns (kit patterns reorders)
  (let ((pats (patterns-with-kit kit patterns)))
    (loop for reorder in reorders
	 do (reorder-kit kit reorder)
	 (loop for pat in pats
	      do (reorder-pattern pat reorder)))))

(defun reorder1 ()
  (dolist (kit *hase-kits*)
    (reorder-kit-patterns kit *hase* '((8 4) (9 0) (10 1) (11 3) (12 5) (13 6) (14 7)))))

(defun reorder2 ()
  (dolist (kit (group-kits (group-with-name "hase2")))
    (reorder-kit-patterns kit (group-patterns (group-with-name "hase2"))
			  '((8 4) (9 0) (10 1) (11 3) (12 5) (13 6) (14 7)))))

(defun renumber-patterns (group start)
  (when (keywordp start)
    (setf start (pattern-num start)))
  (let ((patterns (sort (group-patterns group) #'<
			:key #'(lambda (x)
				 (pattern-num (md-pattern-position x))))))
    (loop for i from start
	 for pat in patterns
	 do (format t "renumbering ~A to ~A~%" (pattern-name (md-pattern-position pat))
		    (pattern-name i))
	 (change-slot-values pat 'position i))
    patterns))

(defun renumber-kits (group &optional (start 0))
  (loop for i from start
       for kit in (group-kits group)
       do (format t "kit ~A at position ~A -> position ~A~%" (kit-name kit) (kit-position kit)
		  i)
       (change-slot-values kit 'position i)))

(defun renumber-songs (group &optional (start 0))
  (loop for i from start
       for song in (group-songs group)
       do (format t "song ~A at position ~A -> position ~A~%" (md-song-name song) (md-song-position song)
		  i)
       (change-slot-values song 'position i)))

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

(defun pattern-images (pattern dir)
  (ensure-directories-exist dir)
  (let* ((kit (md-pattern-kit pattern))
	 (name (symbol-name (pattern-name pattern)))
	 (directory (merge-pathnames (make-pathname :directory (list :relative name))
				     dir))
	 (machines (kit-machines kit))
	 )
    (ensure-directories-exist directory)
    (dolist (machine machines)
      (format t "writing ~A ~A~%" machine (machine-index machine))
      (let* ((num (machine-index machine))
	     (filename (merge-pathnames (format nil "~A-~A.png"
							name num)
						directory)))
	(md-image-file machine filename)))))