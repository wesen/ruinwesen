(in-package :md)

(defun write-text-image (text file &key (scale 1))
  (write-image-to-file file :if-exists :overwrite
		       :image (md-text text :scale scale)))


(defun md-main-page ()
  (start-session)
  (let ((*current-user* (session-value 'user)))
    (if (and (hunchentoot:post-parameter "upload")
	     (listp (hunchentoot:post-parameter "sysex"))
	     (> (length (hunchentoot:post-parameter "sysex")) 1))
	(with-destroying-images ()
	  (handle-upload))
	(with-elektron-page (*standard-output* nil :prologue t :indent t)
	  (:img :src "/files/pngs/upload.png" :border 0 :alt "UPLOAD YOUR SYSEX FILE HERE")
	  :br :br
	  ((:form :method :post :enctype "multipart/form-data")
	   (:input :type "file" :name "sysex" :maxlength "500000") :br :br
	   (:input :type :submit :name "upload" :value "UPLOAD"))))))

(defparameter *last-dir* 0)

(defun find-empty-md-dir ()
  (loop for i from (1+ *last-dir*)
       for dir = (merge-pathnames (make-pathname :directory (list :relative (princ-to-string i)))
				  *elektron-dir*)
     until (not (cl-fad:directory-exists-p dir))
     finally (return (values dir i))))

(defun create-new-md-dir ()
  (multiple-value-bind (dir i) (find-empty-md-dir)
    (setf *last-dir* i)
    (ensure-directories-exist dir)
    (values dir i)))

(defun objects-of-type (list type)
  (remove-if-not #'(lambda (x) (typep x type)) list))

(defun handle-upload ()
  (let ((file (first (hunchentoot:post-parameter "sysex")))
	(filename (second (hunchentoot:post-parameter "sysex"))))
    (handler-case
	(let* ((data (remove-if #'md-object-empty-p (read-elektron-file file))))
	  (multiple-value-bind (dir i) (create-new-md-dir)
	    (let ((kits (objects-of-type data 'kit))
		  (patterns (objects-of-type data 'md-pattern))
		  (songs (objects-of-type data 'md-song)))

	      (write-text-image filename (make-pathname :name (format nil "filename-~A" i)
							:type "gif"
							:defaults *elektron-dir*)
				:scale 3)
	      (with-open-file (s (make-pathname :name (format nil "~A" i)
						:type "html"
						:defaults *elektron-dir*)
				 :direction :output)
		(with-sub-elektron-page (s "SYSEX DUMP" i t)
		  (fmt "~A" (kits-to-html kits dir i))
		  (fmt "~A" (patterns-to-html patterns dir i))
		  (fmt "~A" (songs-to-html songs dir i))))
		  
	      (with-elektron-page (*standard-output* nil :prologue t :indent t)
		(:img :src "/files/pngs/success.png" :border 0
		      :alt "SYSEX PARSED SUCCESSFULLY!") :br :br
		      ((:a :href (format nil "/files/~a.html" i))
		       (:img :src "/files/sps1-uw.jpg" :border 0
			     :alt "image of sps1 uw (c) hageir") :br :br
		       (:img :src (format nil "/files/filename-~a.gif" i)
			     :border 0
			     :alt "FILENAME"))))))
      #+nil(error (e)
	(format t "file: ~A, filename: ~A~%" file filename)
	(format t "erorr; ~A~%" e) (error-page))
      )))

(defun kits-to-html (kits dir i)
  (if (null kits)
      ""
      (with-output-to-string (s)

	(dolist (kit kits)
	  (kit-to-html kit dir i))
	
	(with-html-output (s) :br :br
			  (:img :src "/files/pngs/kits.png" :border 0 :alt "KITS:") :br
			  :br
			  ((:table :style "border-spacing: 15px")
			    (loop for group in (split-into kits 3)
			       do (with-html-output (s)
				    (:tr (loop for kit in group
					      do (with-html-output (s)
						   (:td ((:a :href (format nil "/files/~A/kit~A.html" i (kit-position kit)))
					  (:img :src (format nil "/files/~A/kit~A.gif" i (kit-position kit)) :border 0
						:alt (kit-name kit))))))))))))))

(defun songs-to-html (songs dir i)
  (if (null songs)
      ""
      (with-output-to-string (s)

	(dolist (song songs)
	  (song-to-html song dir i))

	(with-html-output (s) :br :br
			  (:img :src "/files/pngs/songs.png" :border 0 :alt "SONGS:") :br :br
			  ((:table :style "border-spacing: 15px")
			   (loop for group in (split-into songs 3)
				do (with-html-output (s)
				     (:tr (loop for song in group
					       do (with-html-output (s)
						    (:td ((:a :href (format nil "/files/~a/song~a.html" i (md-song-position song)))
							  (:img :src (format nil "/files/~a/song~a.gif" i (md-song-position song)) :border 0
								:alt (md-song-name song))))))))))))))
	 

(defun patterns-to-html (patterns dir i)
  (if (null patterns)
      ""
      (with-output-to-string (s)

	(dolist (pattern patterns)
	  (pattern-to-html pattern dir i))
	
	(with-html-output (s) :br :br
			  (:img :src "/files/pngs/patterns.png" :border 0 :alt "PATTERNS:")
			  :br :br
			  ((:table :style "border-spacing: 15px")
			    (loop for group in (split-into patterns 8)
			       do (with-html-output (s)
				    (:tr (loop for pattern in group
					    do (with-html-output (s)
						 (:td ((:a :href (format nil "/files/~A/pattern-~A.html"
									 i (md-pattern-position pattern)))
						       (:img :src (format nil "/files/pngs/pat-~A-2.png"
									  (md-pattern-position pattern))
							     :border 0
							     :alt (symbol-name (pattern-name (md-pattern-position pattern))))))))))))))))




(defun kit-to-html (kit dir i)
  (let ((name (format nil "kit~A" (kit-position kit))))
    (write-text-image (kit-name kit) (make-pathname :name name :type "gif"
						    :defaults dir) :scale 2)
    (write-text-image (kit-name kit) (make-pathname :name (format nil "~a-4" name) :type "gif"
						    :defaults dir) :scale 4)
    (with-open-file (s (make-pathname :name name :type "html"
				      :defaults dir)
		       :direction :output
		       :if-does-not-exist :create)
      (with-sub-elektron-page (s (format nil "KIT ~A: ~A" (kit-position kit) (kit-name kit)) i nil)
	(:div :align :center
	      (:img :src "/files/pngs/kit.png" :border 0 :alt "KIT") " "
	      (:img :src (format nil "/files/pngs/~A-4.png" (kit-position kit))
		    :border 0 :alt (kit-position kit)) ": "
	      (:img :src (format nil "/files/~A/~a-4.gif" i name)
		    :border 0 :alt (kit-name kit)) :br :br
	      (:img :src "/files/pngs/machines.png"
		    :border 0 :alt "MACHINES:") :br :br
	      (loop for machine in (kit-machines kit)
		 for lfo in (kit-lfos kit)
		 for idx from 0
		 do (write-image-to-file (make-pathname :name (format nil "~a-machine-~A" name idx)
							:type "gif"
							:defaults dir)
					 :image (scale-image (md-machine-img machine lfo idx kit)
							     2))
		    (with-html-output (s)
		      (:img :src (format nil "/files/~a/~a-machine-~a.gif" i name idx)
			    :border 0 :alt (format nil "MACHINE ~A" idx)) :br :br)
		   ))))
      name
  ))

(defun song-to-html (song dir i)
  (let ((name (format nil "song~a" (md-song-position song))))
    (write-text-image (md-song-name song) (make-pathname :name name :type "gif" :defaults dir)
		      :scale 2)
    (write-text-image (md-song-name song) (make-pathname :name (format nil "~a-4" name) :type "gif"
						      :defaults dir) :scale 4)
    (with-open-file (s (make-pathname :name name :type "html"
				      :defaults dir)
		       :direction :output
		       :if-does-not-exist :create)
      (with-sub-elektron-page (s (format nil "SONG ~A: ~A" (md-song-position song) (md-song-name song)) i nil)
	(:div :align :center
	      (:img :src "/files/pngs/song.png" :border 0 :alt "SONG") " "
	      (:img :src (format nil "/files/pngs/~A-4.png" (md-song-position song))
		    :border 0 :alt (md-song-position song)) ": "
		    (:img :src (format nil "/files/~a/~a-4.gif" i name)
			  :border 0 :alt (md-song-name song)) :br :br
	      (write-image-to-file (make-pathname :name (format nil "~a-rows" name)
						  :type "gif"
						  :defaults dir)
				   :image (scale-image (md-song-img song) 2))
	      (with-html-output (s)
		(:img :src (format nil "/files/~a/~a-rows.gif" i name)
		      :border 0 :alt "SONG ROWS") :br :br))))))

(defun pattern-to-html (pattern dir i)
  (with-open-file (s (make-pathname :name (format nil "pattern-~A" (md-pattern-position pattern))
				    :type "html"
				    :defaults dir)
		     :direction :output
		     :if-does-not-exist :create)
    (with-sub-elektron-page (s "PATTERNS" i nil)
      (:div :align :center
	    (:img :src "/files/pngs/pattern.png"
		  :border 0 :alt "PATTERN") " "
	     (:img :src (format nil "/files/pngs/pat-~A-3.png" (md-pattern-position pattern))
		   :border 0 :alt (symbol-name (pattern-name (md-pattern-position pattern))))
	     :br :br
	     (write-image-to-file (make-pathname :name (format nil "pattern-~A" (md-pattern-position pattern))
						 :type "gif"
						 :defaults dir)
				  :image (md-pattern-img pattern))
	     (with-html-output (s)
	       (:img :src (format nil "/files/~a/pattern-~A.gif" i (md-pattern-position pattern))
		     :border 0 :alt (format nil "PATTERN ~A" (symbol-name
							      (pattern-name 
							       (md-pattern-position pattern))))))))))
  
