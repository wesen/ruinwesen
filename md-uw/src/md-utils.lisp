(in-package :md)

(defun get-pattern-by-name (name patterns)
  "Find the pattern named NAME in a list of patterns."
  (find name patterns
        :key #'(lambda (x) (pattern-name (md-pattern-position x)))
	:test #'equal))

(defun get-patterns (names patterns)
  "Get patterns from a list of NAMES."
  (remove-duplicates (mapcar #'(lambda (x) (get-pattern-by-name x patterns)) names)))

(defun show-kit (kit)
  "Print out a kit in a crude way."
  (format t "~%~A~%" (kit-name kit))
  (loop for machine in (sort (copy-tree (kit-machines kit)) #'< :key #'machine-index)
     do (format t "machine ~A: ~A~%" (machine-index machine) (machine-name machine))))

(defun show-pats (patterns)
  (dolist (pat patterns)
    (format t "pat: ~A, kit: ~A~%" (pattern-name (md-pattern-position pat)) (kit-name (md-pattern-kit pat)))
    (show-kit (md-pattern-kit pat))))

(defun patterns-with-kit (kit patterns)
  (remove-if-not #'(lambda (x) (equal kit (md-pattern-kit x))) patterns))

(defun sort-by-index (list)
  (sort (copy-list list) #'< :key #'(lambda (x)
				      (slot-value x 'index))))

(defun reorder-kit (kit reorder)
  (let ((oldpos (first reorder))
	(newpos (second reorder))
	(machines (sort-by-index (kit-machines kit)))
	(lfos (sort-by-index (kit-lfos kit))))
    (change-slot-values (elt machines oldpos) 'index newpos)
    (change-slot-values (elt machines newpos) 'index oldpos)
    (change-slot-values (elt lfos oldpos) 'index newpos)
    (change-slot-values (elt lfos newpos) 'index oldpos)))

(defun reorder-pattern (pattern reorder)
  (let ((oldpos (first reorder))
	(newpos (second reorder))
	(tracks (sort-by-index (md-pattern-tracks pattern))))
    (change-slot-values (elt tracks oldpos) 'index newpos)
    (change-slot-values (elt tracks newpos) 'index oldpos)))

(defun reorder-kit-patterns (kit patterns reorders)
  (let ((pats (patterns-with-kit kit patterns)))
    (loop for reorder in reorders
	 do (reorder-kit kit reorder)
	 (loop for pat in pats
	      do (reorder-pattern pat reorder)))))

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
  "Get all the kits out of a group or a list of patterns"
  (let ((patterns (if (typep patterns-or-group 'group)
		      (group-patterns patterns-or-group)
		      patterns-or-group)))
    (remove-duplicates (mapcar #'md-pattern-kit patterns))))

(defun kit-rom-machines (kit)
  (remove-if-not #'rom-model-p (kit-machines kit) :key #'machine-model))

(defun kit-samples (kit)
  (remove-duplicates (mapcar #'machine-sample (kit-rom-machines kit))
		     :test #'string-equal))

(defun pattern-samples (bla)
  (remove-duplicates (mapcan #'kit-samples (collect-machines (list bla)))
		     :test #'string-equal))

(defun import-file (file samples)
  (remove-if #'md-object-empty-p
	     (read-elektron-file file :samples samples)))

(defmethod song-patterns ((song md-song))
  (let ((rows (md-song-rows song)))
    (remove :LOOP (remove :END (remove :JUMP (remove-duplicates
					      (mapcar #'md-row-pattern rows)))))))

(defun patterns-using-rom (patterns rom-num)
  (loop for pat in patterns
     for kit = (md-pattern-kit pat)
     for rom-machines = (kit-rom-machines kit)
     for nums = (mapcar #'rom-model-num (mapcar #'machine-model rom-machines))
     when (member (1- rom-num)  nums)
     collect pat))

(defmethod replace-sample ((kit kit) old-sample new-sample)
  (loop for machine in (kit-machines kit)
       when (and (machine-sample machine) 
		 (string= (pathname-name (machine-sample machine))
			  (pathname-name old-sample)))
       do (format t "replacing sample ~A of machine ~A of kit ~A with ~A~%"
		  old-sample (machine-index machine) (kit-name kit) new-sample)
       (change-slot-values machine 'sample new-sample)))

(defmethod replace-sample ((group group) old-sample new-sample)
  (dolist (kit (group-kits group))
    (replace-sample kit old-sample new-sample)))

(defmethod set-sample-position ((kit kit) sample index)
  (loop for machine in (kit-machines kit)
       when (and (machine-sample machine)
		 
		 (string= (pathname-name (machine-sample machine)) (pathname-name sample)))
     do (format t "assigning sample ~A to model ~A on machine ~A of kit ~A~%"
		sample (machine-name (rom-num-model index))
		(machine-index machine) (kit-name kit))
       (change-slot-values machine 'model (rom-num-model index))))

(defmethod set-sample-position ((group group) sample index)
  (dolist (kit (group-kits group))
    (set-sample-position kit sample index)))


(defmethod update-with-sample-list ((kit kit) sample-list)
  (loop for sample in (kit-samples kit)
     do (let ((position (index-of sample-list (pathname-name sample)
				  :test #'string= :key #'pathname-name)))
	  (set-sample-position kit sample position))))

(defmethod update-with-sample-list ((group group) sample-list)
  (dolist (kit (group-kits group))
    (update-with-sample-list kit sample-list)))


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