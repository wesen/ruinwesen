(in-package :md)

(defun read-ableton-file (file)
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s nil :eof)
	 until (eq line :eof)
	 collect line)))

(defun print-ableton-scene (scene)
  (format t "~3A: " (first scene))
  (loop for (track pattern) in (cdr scene)
       if (first pattern)
       do (format t "~A-~2D" (first pattern) (second pattern))
       else
       do (format t "      ")
       do (format t " | "))
  (Format t "~%"))

(defvar *color-hash*)

(defun print-ableton-scene-html (scene)
  (with-html-output (*standard-output* nil)
    (:tr (:td (princ (first scene)))
	 (loop for (track pattern) in (cdr scene)
	    do (let ((color (gethash (first pattern) *color-hash*)))
		 (unless color
		   (setf color (random-color))
		   (setf (gethash (first pattern) *color-hash*) color))
		 (with-html-output (*standard-output* nil)
		   (:td ((:div :style (format nil "background-color: #~A" color))
			 (princ (if pattern
				    (format nil "~A-~A<br/>~A"
					    (first pattern) (second pattern)
					    (third pattern))
				    ""))))))))))

#+nil(defparameter *colors*
  '("aqua" "black" "blue" "fuchsia" "gray" "green"
    "lime" "maroon" "navy" "olive" "purple" "red"
    "silver" "teal" "white" "yellow"))

(defun parse-track (string group)
  (multiple-value-bind (res arr)
      (cl-ppcre:scan-to-strings "\([A-H][0-9]+\)-\([0-9]+\)" string)
    (when res
      (find-pattern-track
       (make-keyword-from-string (aref arr 0))
       (parse-integer (aref arr 1)) (group-patterns group)))))

(defun match-to-strings (regex string)
  (multiple-value-bind (res arr)
      (cl-ppcre:scan-to-strings regex string)
    (when res
      (coerce arr 'list))))

(defun parse-pattern-description (string group)
  (let* ((patterns (group-patterns group))
	 (basepat-tracknames-newpat (match-to-strings "\([A-H][0-9]+\) : \(.*\) -> \([A-H][0-9]+\) \([0-9]+\)" string))
	 (tracknames (cl-ppcre:split "\\s+" (second basepat-tracknames-newpat)))
	 (basename  (make-keyword-from-string
		     (first basepat-tracknames-newpat)))
	 (basekit (find-pattern-kit basename patterns))
	 (dest-pos (pattern-num (make-keyword-from-string (third basepat-tracknames-newpat))))
	 (dest-kitpos (parse-integer (fourth basepat-tracknames-newpat)))
	 (tracks (mapcar #'(lambda (x) (parse-track x group)) tracknames))
	 (machines (mapcar #'(lambda (x)
			       (when x (md-pattern-track-machine x))) tracks))
	 (newkit (copy-store-object basekit))
	 (pattern (merge-patterns (list (find-pattern basename patterns)))))

    (change-slot-values newkit 'machines machines)
    (fill-kit-with-empty-machines newkit)
    
    (setf tracks (mapcar #'copy-store-object-recurse tracks))
    (dolist (track tracks)
      (when (and track (= (track-length track) (/ (md-pattern-length pattern) 2)))
	(track-double-length track))
      (change-slot-values track 'pattern pattern))
    (change-slot-values pattern 'kit newkit
			'tracks tracks)
    (fill-pattern-with-empty-tracks pattern)
    (format t "writing ~A to pos ~A and kit ~A to pos ~A~%"
	    pattern dest-pos
	    newkit dest-kitpos)
    (write-pat-kit-to-sysex pattern dest-pos dest-kitpos)
    pattern))

(defun parse-pattern-file (file group)
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s nil :eos)
	 until (eql line :eos)
	 do (parse-pattern-description line group))))

(defparameter *kit-description*
      "B05 : B07-1 NIL B07-3 B07-4 NIL B07-6 nil B05-8 B05-9 B05-10 B05-11 -> F01 32")

(defparameter *kit-description*
      "B07 : B07-1 NIL B07-3 B07-4 NIL B07-6 nil B05-8 B05-9 B05-10 B05-11 -> F01 32")

(defparameter *colors*
  '("330000" "333300" "336600" "339900" "33cc00" "33ff00"
    "660000" "663300" "666600" "669900" "66cc00" "66ff00"
    "ff0000" "ff3300" "ff6600" "ff9900" "ffcc00" "ffff00"

    "3300CC" "3333CC" "3366CC" "3399CC" "33ccCC" "33ffCC"
    "6600CC" "6633CC" "6666CC" "6699CC" "66ccCC" "66ffCC"
    "ff00CC" "ff33CC" "ff66CC" "ff99CC" "ffccCC" "ffffCC"    

    "0000CC" "0033CC" "0066CC" "0099CC" "00ccCC" "00ffCC"
    "9900CC" "9933CC" "9966CC" "9999CC" "99ccCC" "99ffCC"
    "cc00CC" "cc33CC" "cc66CC" "cc99CC" "ccccCC" "ccffCC"    

    "000000" "003300" "006600" "009900" "00cc00" "00ff00"
    "990000" "993300" "996600" "999900" "99cc00" "99ff00"
    "cc0000" "cc3300" "cc6600" "cc9900" "cccc00" "ccff00"))

(defun random-color (&optional used-colors)
  (declare (ignore used-colors))
  (elt *colors* (random (length *colors*))))  

(defun print-ableton-html (scenes &optional (s *standard-output*))
  (let ((*color-hash* (make-hash-table)))
    (with-html-output (*standard-output* s)
      (:html
       (:body
	((:table :border 1)
	 (princ (map nil #'print-ableton-scene-html scenes))))))))

(defun print-ableton-file (scenes)
  (loop for scene in scenes
       do (print-ableton-scene scene)))

(defun parse-ableton-file (file)
  (let ((lines (read-ableton-file file)))
    (loop with scenes = (list)
	 with scene = nil
	 for line in lines
       do
;	 (format t "line: ~A~%" line)
	 (multiple-value-bind (res elts)
	      (cl-ppcre:scan-to-strings "Scene[^0-9]*\([0-9]+\)" line)
	    (when res
	      (push (nreverse scene) scenes)
	      (setf scene (list (aref elts 0)))))
	 (multiple-value-bind (res elts)
	     (cl-ppcre:scan-to-strings "Track \([0-9]+\): \(.*\)" line)
;	   (format t "second: ~A~%" res)
	   (when res
	     (push (list (parse-integer (aref elts 0))
			 (clip-to-track (aref elts 1))) scene)))
       finally (push (nreverse scene) scenes) (return (nreverse scenes)))))

(defun clip-to-track (clipname)
  (if (string= clipname "no clip")
      nil
      (multiple-value-bind (res elts)
	  (cl-ppcre:scan-to-strings "Clip '\([A-Z0-9]+\) \([0-9]+\) \(.*\)'"
				    clipname)
	(when res
	  (list (make-keyword-from-string (aref elts 0))
		(parse-integer (aref elts 1))
		(aref elts 2))))))

(defun track-description (track)
  (second track))

(defun track-empty-p (track)
  (null (track-description track)))

(defun scene-name (scene)
  (first scene))

(defun scene-tracks (scene)
  (cdr scene))

(defun scene-empty-p (scene)
  (every #'track-empty-p (scene-tracks scene)))

(defun ableton-track-machine (track patterns)
  (let ((desc (second track)))
    (when desc
      (find-pattern-track-machine (first desc) (second desc) patterns))))

(defun ableton-md-track (track patterns)
  (let ((desc (second track)))
    (when desc
      (find-pattern-track (first desc) (second desc) patterns))))

(defun ableton-scene-machines (scene patterns)
  (fill-list (mapcar #'(lambda (x) (ableton-track-machine x patterns))
		     (scene-tracks scene)) 16))

(defun ableton-scene-tracks (scene patterns)
  (fill-list (mapcar #'(lambda (x) (ableton-md-track x patterns))
		     (scene-tracks scene)) 16))

(defun all-equal (elts &key (test #'equal))
  (every #'(lambda (x) (funcall test (first elts) x)) (cdr elts)))

(defun merge-kits (kits &key machines lfos name)
  (let ((reverbs (mapcar #'kit-reverb kits))
	(eqs (mapcar #'kit-eq kits))
	(delays (mapcar #'kit-delay kits))
	(dynamics (mapcar #'kit-dynamics kits)))

    (when (null name)
      (setf name (reduce #'(lambda (x1 x2)
			     (concatenate 'string x1 x2))
			 (mapcar #'kit-name kits))))

    (unless (all-equal reverbs)
      (warn "not all reverbs equal, using first"))
    (unless (all-equal eqs)
      (warn "not all eqs equal, using first"))
    (unless (all-equal delays)
      (warn "not all delays equal, using first"))
    (unless (all-equal dynamics)
      (warn "not all dynamics equal, using first"))
    (make-object 'kit
		 :name name
		 :machines machines
		 :lfos lfos
		 :reverb (first reverbs)
		 :eq (first eqs)
		 :delay (first delays)
		 :dynamics (first dynamics))))

(defun fill-list (list upto &key initial-element)
  (when (< (length list) upto)
    (append list (make-list (- upto (length list))
			    :initial-element initial-element))))

(defun ableton-scene-kit (scene patterns)
  (let* ((machines (ableton-scene-machines scene patterns))
	 (kit (merge-kits (mapcar #'machine-kit (remove nil machines)))))
    (change-slot-values kit 'machines machines
			'lfos (mapcar #'(lambda (x) (when x (machine-lfo x)))
				      machines)
			'name (format nil "S~A" (scene-name scene)))
    (fill-kit-with-empty-machines kit)
    kit))

(defun merge-patterns (patterns &key tracks position)
  (let ((accent-tracks (mapcar #'md-pattern-accent-track patterns))
	(accents (mapcar #'md-pattern-accent patterns))
	(swing-tracks (mapcar #'md-pattern-swing-track patterns))
	(swings (mapcar #'md-pattern-swing patterns))
	(slide-tracks (mapcar #'md-pattern-slide-track patterns))
	(lengths (mapcar #'md-pattern-length patterns))
	(scales (mapcar #'md-pattern-scale patterns))
	(kits (mapcar #'md-pattern-kit patterns))
	accent-track accent swing-track swing length scale slide-track kit)
    (unless (all-equal accent-tracks)
      (warn "not all accent tracks equal, using first"))
    (setf accent-track (first accent-tracks))
    (unless (all-equal accents)
      (warn "not all accent values equal, using max"))
    (setf accent (reduce #'max accents))
    (unless (all-equal swing-tracks)
      (warn "not all swing tracks equal, using first"))
    (setf swing-track (first swing-tracks))
    (unless (all-equal swings)
      (warn "not all swing values equal, using max"))
    (setf swing (reduce #'max swings))
    (unless (all-equal slide-tracks)
      (warn "not all slide tracks equal, using first"))
    (setf slide-track (first slide-tracks))
    (unless (all-equal lengths)
      (warn "not all lengths equal, using max")
      ;;; XXX adapt tracks length
      )
    (setf length (reduce #'max lengths))
    (unless (all-equal scales)
      (warn "not all scales equal, using max"))
    (setf scale (reduce #'max scales))
    (unless (all-equal kits)
      (warn "not all kits equal, using first"))
    (setf kit (first kits))
    (make-object 'md-pattern
		 :accent-track accent-track
		 :accent accent
		 :swing-track swing-track
		 :swing swing
		 :slide-track slide-track
		 :scale scale
		 :double-tempo 0
		 :length length
		 :tracks tracks
		 :kit kit
		 :position position)))

(defun double-list (list length)
  (append (subseq list 0 length)
	  (subseq list 0 length)
	  (when (< (* 2 length) (length list))
	    (subseq list (* 2 length)))))

(defun track-double-length (track)
  (let ((length (track-length track)))
    (with-slots (hits accent swing slide plock-pattern plocks) track
	(change-slot-values track
			    'hits (double-list hits length)
			    'accent (double-list accent length)
			    'swing (double-list swing length)
			    'slide (double-list slide length)
			    'plock-pattern (double-list plock-pattern length))
      (dolist (plock plocks)
	(change-slot-values plock
			    'vals (double-list (plock-vals plock) length)))))
  track)

(defun pattern-double-length (pattern)
  (dolist (track (md-pattern-tracks pattern))
    (track-double-length track))
  (with-slots (accent-track slide-track swing-track length) pattern
    (change-slot-values pattern
			'accent-track (double-list accent-track length)
			'slide-track (double-list slide-track length)
			'swing-track (double-list swing-track length)
			'length (* 2 length))))

(defmethod copy-object ((object t))
  (copy-tree object))

(defun ableton-scene-pattern (scene patterns &key position)
  (let* ((tracks (ableton-scene-tracks scene patterns))
	 (pattern (merge-patterns (mapcar #'md-pattern-track-pattern
					  (remove nil tracks)) :position position)))
    (setf tracks (mapcar #'copy-store-object-recurse tracks))
    (dolist (track tracks)
      (when (and track (= (track-length track) (/ (md-pattern-length pattern) 2)))
	(track-double-length track))
      (change-slot-values track 'pattern pattern))
    (change-slot-values pattern 'kit (ableton-scene-kit scene patterns)
			'tracks tracks)
    (fill-pattern-with-empty-tracks pattern)
    pattern))

;;; XX make empty machines and empty tracks

(defun elt-subset-p (e1 e2 &key (test #'eql))
  (or (null e1) (null e2) (md-object-empty-p e1)
      (md-object-empty-p e2)
      (funcall test e1 e2)))

(defun elt-subset-<= (e1 e2 &key (test #'eql))
  (or (null e1) (md-object-empty-p e1) (funcall test e1 e2)))

(defun list-subset-p (l1 l2)
  (and (= (length l1) (length l2))
       (every #'elt-subset-p l1 l2)))

(defun list-subset-<= (l1 l2)
  (and (= (length l1) (length l2))
       (every #'elt-subset-<= l1 l2)))

(defun merge-subset-lists (l1 l2)
  (loop for e1 in l1
       for e2 in l2
       collect (if (or (null e1) (md-object-empty-p e1)) e2
		   e1)))

(defun kit-subset-p (k1 k2)
  (list-subset-p (kit-machines k1) (kit-machines k2)))

(defun kit-subset-<= (k1 k2)
  (list-subset-<= (kit-machines k1) (kit-machines k2)))

(defun pattern-subset-p (p1 p2)
  (and (list-subset-p (md-pattern-tracks p1) (md-pattern-tracks p2))
       (kit= (md-pattern-kit p1) (md-pattern-kit p2))))

(defun merge-patterns-tracks (p1 p2)
  (let* ((tracks (merge-subset-lists (md-pattern-tracks p1)
				     (md-pattern-tracks p2))))
    (merge-patterns (list p1 p2) :tracks tracks)))

(defun merge-kits-machines (k1 k2)
  (let* ((machines (merge-subset-lists (kit-machines k1) (kit-machines k2)))
	 (lfos (mapcar #'machine-lfo machines)))
    (merge-kits (list k1 k2) :machines machines :lfos lfos)))

(defun reduce-kits (patterns)
  (let* ((kits (mapcar #'md-pattern-kit patterns))
	 (kit-arr (make-array (length kits)))
	 (kit-hash (make-hash-table)))
    (loop for i from 0
       for kit in kits
       do (setf (aref kit-arr i) kit
		(gethash kit kit-hash) kit))
    (loop for i from 0 below (length kit-arr)
       for k1 = (aref kit-arr i)
       do (loop for j from (1+ i) below (length kit-arr)
	     for k2 = (aref kit-arr j)
	     when (and (not (= i j)) (not (eql k1 k2))
		       (kit-subset-p k1 k2))
	     do (let ((knew (cond ((kit-subset-<= k1 k2) k2)
				  ((kit-subset-<= k2 k1) k1)
				  (t (merge-kits-machines k1 k2)))))
		  (format t "merging kit ~A  ~A and kit ~A ~A~%"
			  i (kit-name k1)
			  j (kit-name k2))
		  (loop for k being the hash-key of kit-hash
		       for v being the hash-value of kit-hash
		       when (or (eql v k1) (eql v k2))
		       do (setf (gethash k kit-hash) knew))
		  (setf (aref kit-arr i) knew
			(aref kit-arr j) knew
			k1 knew))))
    kit-hash))

(defun reduce-patterns (patterns)
  (let ((pat-arr (make-array (length patterns)))
	(pat-hash (make-hash-table)))
    (loop for i from 0
	 for pat in patterns
	 do (setf (aref pat-arr i) pat
		  (gethash pat pat-hash) pat))
    (loop for i from 0 below (length pat-arr)
       for p1 = (aref pat-arr i)
       do (loop for j from (1+ i) below (length pat-arr)
	     for p2 = (aref pat-arr j)
	     when (and (not (= i j)) (not (eql p1 p2))
		       (pattern-subset-p p1 p2))
	     do (let ((pnew (merge-patterns-tracks p1 p2)))
		  (format t "merging pattern ~A and pattern ~A~%" i j)
		  ;; (describe pnew)
		  (loop for k being the hash-key of pat-hash
		       for v being the hash-value of pat-hash
		       when (or (eql v p1) (eql v p2))
		       do (setf (gethash k pat-hash) pnew))
		  (setf (aref pat-arr i) pnew
			(aref pat-arr j) pnew
			p1 pnew))))
    pat-hash))
			

(defun scene-to-mutes (scene)
  (fill-list (mapcar #'(lambda (x) (if (cadr x) 0 1)) (scene-tracks scene)) 16 :initial-element 0))

(defun ableton-scene-to-row (scene patterns &key song index position)
  (let ((pattern (ableton-scene-pattern scene patterns :position position))
	(mutes (scene-to-mutes scene)))
    (make-object 'md-row :song song
		 :index index
		 :pattern pattern
		 :kit (md-pattern-kit pattern)
		 :start 0
		 :stop (md-pattern-length pattern)
		 :mutes mutes
		 :loop-times 31)))

(defun replace-row-kit (rows oldkit newkit)
  (loop for row in rows
       for pattern = (md-row-pattern row)
       do (when (eql (md-row-kit row) oldkit)
	    (change-slot-values row 'kit newkit))
       (when (and (typep pattern 'md-pattern)
		  (eql (md-pattern-kit pattern) oldkit))
	 (change-slot-values pattern 'kit newkit))))
		     

(defun replace-row-pattern (rows oldpat newpat)
  (loop for row in rows
       do (when (eql (md-row-pattern row) oldpat)
	    (change-slot-values row 'pattern newpat))))

(defun renumber-kits (kits &key (start 0))
  (loop for i from start
       for kit in (remove-duplicates kits)
       do (change-slot-values kit 'position i)))

(defun renumber-patterns (patterns &key (start 0))
  (loop for i from start
       for pattern in (remove-duplicates patterns)
       do (change-slot-values pattern 'position i)))


(defun ableton-scenes-to-song (scenes patterns &key (name "SONG")
			       (start-kit 0) (start-pattern 0))
  (let* ((song (make-object 'md-song
			    :name name))
	 (scene-rows  (loop for i from 0
			  for scene in scenes
			  collect (ableton-scene-to-row scene patterns
							:song song
							:index i)))
	 (rows (append scene-rows
		       (list (make-end-row song (1+ (length scene-rows))
					   (md-row-kit (first scene-rows))))))
	 (patterns (mapcar #'md-row-pattern scene-rows)))
;    #+nil
    (let ((kit-hash (reduce-kits patterns)))
      (loop for k being the hash-key of kit-hash
	   for v being the hash-value of kit-hash
	   do (replace-row-kit rows k v)))
;    #+nil
    (let ((pat-hash (reduce-patterns patterns)))
      (loop for k being the hash-key of pat-hash
	 for v being the hash-value of pat-hash
	   do (replace-row-pattern rows k v)))

    (renumber-patterns (mapcar #'md-row-pattern scene-rows)
		       :start start-pattern)
    (renumber-kits (append (mapcar #'md-row-kit rows)
			   (mapcar #'(lambda (x) (md-pattern-kit
						  (md-row-pattern x)))
				   scene-rows))
		   :start start-kit)
    (change-slot-values song 'rows rows)
    song))
  