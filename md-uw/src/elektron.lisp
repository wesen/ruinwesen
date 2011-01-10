
(in-package :md)

;;;; elektron specific messages

(defparameter *elektron-sysex-header* '(#xf0 #x00 #x20 #x3c #x02 #x00))
#+nil(defparameter *elektron-sysex-header* '(#xf0 #x00 #x20 #x02 #x00 #x7f))

(defun set-current-kit-name (name)
  (let ((msg (append *elektron-sysex-header*
		     (list #x55)
		     (string-to-sysex name)
		     (list #xf7))))
    (write-sysex *output* msg)))

(defun load-pattern (pattern)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x57 pattern #xf7))))
(defun load-kit (pos)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x58 pos #xf7))))

(defun save-current-kit-to-pos (pos)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x59 pos #xf7))))

(defun assign-machine (track machine &key (md-uw 0) (init-type 0))
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x5b track machine md-uw init-type #xf7))))

(defun set-delay-param (param value)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x5d param value #xf7))))

(defun set-reverb-param (param value)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x5e param value #xf7))))

(defun set-eq-param (param value)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x5f param value #xf7))))

(defun set-compressor-param (param value)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x60 param value #xf7))))

(defun set-tempo (tempo)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x61)
				(short-to-sysex (* tempo 24))
				(list #xf7))))

(defun set-lfo (lfo param value)
  (let ((byte (let ((res 0))
		(setf (ldb (byte 4 0) res) param
		      (ldb (byte 4 4) res) lfo)
		res)))
    (write-sysex *output* (append *elektron-sysex-header*
				  (list #x62 byte value #xf7)))))

(defun set-receive-position (type pos count)
  (let ((type-byte (case type
		     (:GLOBAL 1)
		     (:KIT 2)
		     (:PATTERN 4)
		     (:SONG 8)))
	(pos-byte (if (eql pos :ORIG)
		      #b1111111
		      pos)))
    (write-sysex *output* (append *elektron-sysex-header*
				  (list #x6b type-byte pos-byte count #xf7)))))

(defun load-song (song)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x6c song #xf7))))

(defun save-current-song (song)
  (write-sysex *output* (append *elektron-sysex-header*
				(list #x6d song #xf7))))

(defun get-request (type request)
  (flush *input*)
  (let ((msg (append *elektron-sysex-header*
		     (list type request #xf7))))
    (write-sysex *output* msg)
    (let ((data (read-sysex *input*)))
      (with-get-byte-from-arr (s data)
	(read-header s)
	(read-type s)))))

(defun get-kit (kit)
  (get-request #x53 kit))

(defun get-pattern (pattern)
  (get-request #x68 pattern))

(defun get-song (song)
  (get-request #x6a song))

;;; parse elektron data

(define-condition wrong-header (condition)
  ())

(defun read-header (s)
  (let ((header (get-bytes s 6)))
    #+nil(unless (equal header *elektron-sysex-header*)
      (error (make-condition 'wrong-header)))))

(defun write-header (s)
  (put-bytes *elektron-sysex-header* s))

(defun read-global-msg (s)
  #+nil(format t "read-global-msg~%")
  (get-bytes s (- #xc4 6))
  nil)

(defun read-type (s)
  (case (get-byte s)
    (#x50 (read-global-msg s))
    (#x52 (read-kit-msg s))
    (#x67 (read-pattern-msg s))
    (#x69 (read-song-msg s))
    (t (error "Unknown message"))))

(defvar *samples* nil)
(defvar *pattern-hash* nil)

(defun read-elektron-file (file &key position samples)
  (with-open-file (s file :direction :input :element-type 'unsigned-byte)
    (when position
      (file-position s position))
    (let ((*kit-hash* (make-hash-table))
	  (*pattern-hash* (make-hash-table))
	  (*samples* (if (stringp samples)
			 (mapcar #'namestring
				 (directory samples)
				 #+nil(append (directory (format nil "~A/*.wav" samples))
					 (directory (format nil "~A/*.sds" samples))
					 #+nil(directory (format nil "~A/*.syx" samples))
					 (directory (format nil "~A/*.aif" samples))))
			 samples)))
      (format t "SMAPZ ~A~%" *samples*)
      (remove nil
	      (loop for i from 0
	       with res = (list)
		 do (progn
		      (format t "file position ~A~%" (file-position s))
		      (handler-case (progn
				      (read-header s)
				      (push (read-type s) res))
			(end-of-file () (return (nreverse res)))
			(wrong-header () (warn "wrong header, skipping object")
			       (read-until s #xf7))
			)))))))

(defun split-beat (track)
  (loop for i in (reverse track)
       nconc (split-byte i)))

(defun beat-to-bytes (track)
  (loop with res = 0
     for bit from 0
     for hit in track
     do (setf (ldb (byte 1 bit) res) hit)
       finally (return (if (= (length track) 16)
			   (list (ldb (byte 8 8) res)
				 (ldb (byte 8 0) res))
			   (list (ldb (byte 8 24) res)
				 (ldb (byte 8 16) res)
				 (ldb (byte 8 8) res)
				 (ldb (byte 8 0) res))))))

(defun split-lock-pattern (pattern)
  (append (split-byte (fourth pattern))
	  (split-byte (third pattern))
	  (split-byte (second
		       pattern))))

(defun read-kit-msg (s)
  #+nil(format t "read-kit-msg~%")
  (get-bytes s 2)
  (let* ((pos (get-byte s))
	 (name (make-c-string (get-bytes s 16)))
	 (track-params (repeat-collect 16 (get-bytes s 24)))
	 (levels (repeat-collect 16 (get-byte s)))
	 (models (mapcar #'make-long (split-into (read-sysex-bytes s 64) 4)))
	 (lfo-data (split-into (read-sysex-bytes s (* 16 36)) 36))
	 (reverb (get-bytes s 8))
	 (delay (get-bytes s 8))
	 (eq (get-bytes s 8))
	 (dynamics (get-bytes s 8))
	 (trig-data (split-into (read-sysex-bytes s 32) 16))
	 (trigs (first trig-data))
	 (mutes (second trig-data))

	 (tracks (loop for track-param in track-params
		    for model in models
		    for level in levels
		    for trig in trigs
		    for mute in mutes
		    for index from 0
		    collect (make-machine track-param index model level trig mute)))
	 (lfos (loop for data in lfo-data
		  for index from 0
		  collect (make-lfo index data))))

    (get-bytes s (- #x4d0 #x4cb))

    (let ((kit (make-instance 'kit
			    :position pos
			    :name name
			    :machines tracks
			    :lfos lfos
			    :reverb reverb
			    :delay delay
			    :eq eq
			    :dynamics dynamics)))
      (loop for lfo in lfos
	 for machine in tracks
	 do (change-slot-values machine 'lfo lfo 'kit kit)
	   (change-slot-values lfo 'kit kit))
      kit)))

(defun make-trig-data (tracks)
  (with-put-byte-to-list (s)
    (dolist (track tracks)
      (put-byte (machine-trig track) s))
    
    (dolist (track tracks)
      (put-byte (machine-mute track) s))))

(defun lfo-to-data (lfo)
  (with-put-byte-to-list (s)
    (with-slots (destination param shape1 shape2 type state) lfo
      (put-byte destination s)
      (put-byte param s)
      (put-byte shape1 s)
      (put-byte shape2 s)
      (put-byte type s)
      (put-bytes state s))))
  
(defun write-kit-msg (kit)
  (with-put-byte-to-list (s)
    (write-header s)
    (put-byte #x52 s)
    (put-bytes '(#x04 #x01) s)
    (start-checksum s)
    (put-byte (kit-position kit) s)
    (put-bytes (string-to-sysex (kit-name kit)) s)
    (let ((tracks (sort (copy-tree (kit-machines kit)) #'< :key #'machine-index)))
      (dolist (track tracks)
	(put-bytes (machine-params track) s))
      (put-bytes (mapcar #'machine-level tracks) s)
      
      (put-bytes (data-to-sysex (mapcan #'long-to-bytes (mapcar #'(lambda (x)
								    (machine-num (machine-model x)))
								tracks))) s)
      (put-bytes (data-to-sysex (mapcan #'lfo-to-data (sort (copy-tree (kit-lfos kit)) #'< :key #'lfo-index))) s)
      
      (put-bytes (kit-reverb kit) s)
      (put-bytes (kit-delay kit) s)
      (put-bytes (kit-eq kit) s)
      (put-bytes (kit-dynamics kit) s)
      
      (put-bytes (data-to-sysex (make-trig-data tracks)) s))
    (stop-checksum s)
    (put-bytes (get-checksum s) s)
    (put-bytes (short-to-sysex (- (get-length s) 7)) s)
    (put-byte #xf7 s)))

(defun write-msg-to-file (msg file)
  (with-open-file (s2 file :direction :output
		      :if-exists :supersede
		      :element-type '(unsigned-byte 8))
    (my-write-bytes msg s2)))

(defun write-msg-to-sysex (msg)
  (write-sysex *output* msg))

(defun write-kit-to-file (kit file)
  (write-msg-to-file (write-kit-msg kit) file))

(defun write-kit-to-sysex (kit)
  (write-msg-to-sysex (write-kit-msg kit)))

(defun write-pat-kit-to-sysex (pattern patposition kitposition)
  (let ((kit (md-pattern-kit pattern)))
    (change-slot-values pattern 'position (pattern-num patposition))
    (change-slot-values kit 'position kitposition)
    (write-kit-to-sysex kit)
    (write-pattern-to-sysex pattern)))

;;; pattern message

(defun track-make-lock-pattern (track)
  (let ((res 0))
    (dolist (plock (md-pattern-track-plocks track))
      (setf (ldb (byte 1 (plock-param plock)) res) 1))
    (long-to-bytes res)))

(defun track-make-lock (track)
  (let ((plocks (sort (copy-tree (md-pattern-track-plocks track)) #'< :key #'plock-param)))
    (mapcan #'(lambda (x) (copy-tree (plock-vals x))) plocks)))

(defun write-pattern-msg (pat)
  (with-put-byte-to-list (s)
    (write-header s)
    (put-byte #x67 s)
    (put-bytes '(#x02 #x01) s)
    (start-checksum s)
    (put-byte (pattern-num (md-pattern-position pat)) s)
    (let ((tracks (sort (copy-tree (md-pattern-tracks pat)) #'< :key #'md-pattern-track-index)))
      (put-bytes (data-to-sysex (mapcan #'beat-to-bytes (mapcar #'md-pattern-track-hits tracks))) s)
      (put-bytes (data-to-sysex (mapcan #'track-make-lock-pattern tracks)) s)
      (put-bytes (data-to-sysex
		  (append (beat-to-bytes (md-pattern-accent-track pat))
			  (beat-to-bytes (md-pattern-slide-track pat))
			  (beat-to-bytes (md-pattern-swing-track pat))
			  (long-to-bytes (* 328 (- (md-pattern-swing pat) 50))))) s)
      (put-byte (md-pattern-accent pat) s)
      (put-byte (md-pattern-length pat) s)
      (put-byte (md-pattern-double-tempo pat) s)
      (put-byte (md-pattern-scale pat) s)
      (put-byte (let ((kit (md-pattern-kit pat)))
		  (if (typep kit 'kit)
		      (kit-position kit)
		      kit)) s)
      (put-byte 0 s) ;; locked rows
      (put-bytes (data-to-sysex (pad-to (mapcan #'track-make-lock tracks) (* 64 32) :val #xFF)) s)
      (put-bytes (data-to-sysex (append (long-to-bytes 1)
					(long-to-bytes 1)
					(long-to-bytes 1)
					(mapcan #'beat-to-bytes (mapcar #'md-pattern-track-accent tracks))
					(mapcan #'beat-to-bytes (mapcar #'md-pattern-track-slide tracks))
					(mapcan #'beat-to-bytes (mapcar #'md-pattern-track-swing tracks))))
		 s))
    (stop-checksum s)
    (put-bytes (get-checksum s) s)
    (put-bytes (short-to-sysex (- (get-length s) 7)) s)
    (put-byte #xf7 s)))
  
(defun write-pattern-to-file (pat file)
  (write-msg-to-file (write-pattern-msg pat) file))

(defun write-pattern-to-sysex (pat)
  (write-msg-to-sysex (write-pattern-msg pat)))

(defun read-until (s byte)
  (loop for b = (get-byte s)
       until (= b byte)))

(defun read-pattern-msg (s)
  #+nil(format t "read-pattern-msg~%")
  (get-bytes s 2)
  (let* ((pos (get-byte s))
	 (track-data (mapcar #'split-beat (split-into (read-sysex-bytes s 64) 4)))
	 (lock-data (mapcar #'split-beat (split-into (read-sysex-bytes s 64) 4)))
	 (accent-data (split-into (read-sysex-bytes s 16) 4))
	 (accent-track (split-beat (first accent-data)))
	 (slide-track (split-beat (second accent-data)))
	 (swing-track (split-beat (third accent-data)))
	 (swing-amount (+ 50 (ceiling (/ (make-long (fourth accent-data)) 328))))
	 (accent-amount (get-byte s))
	 (pattern-length (get-byte s))
	 (double-tempo (get-byte s))
	 (scale (get-byte s))
	 (kit (get-byte s))
	 (locked-rows (get-byte s))
	 (locks (split-into (read-sysex-bytes s (* 64 32)) 32))
	 (extra (read-sysex-bytes s (* 51 4)))
	 (track-accents (mapcar #'split-beat (split-into (subseq extra
								 12
								 (+ 12 (* 16 4)) ) 4)))
	 (track-slides (mapcar #'split-beat (split-into (subseq extra
								(+ 12 (* 16 4))
								(+ 12 (* 16 4 2))) 4)))
	 (track-swings (mapcar #'split-beat (split-into (subseq extra
								(+ 12 (* 16 4 2))
								(+ 12 (* 16 4 3))) 4)))

	 (tracks (loop for data in track-data
		    for plock in lock-data
		    for accents in track-accents
		    for slides in track-slides
		    for swings in track-swings
		    for index from 0
		    collect (make-md-pattern-track nil index data plock accents slides swings))))

    (loop for (track-num param) in (collect-plocks-info tracks)
       for lock in locks
       for track = (elt tracks track-num)
       do (with-slots (plocks) track
	    (change-slot-values track
				'plocks (cons
					 (make-instance 'plock :track track
						      :param param
						      :vals lock) plocks))))
    #+nil(format t "locks: ~A~%~A~%" (track-plocks (first tracks))
		 (track-plocks (second tracks)))
    (read-until s #xf7)
    #+nil(get-bytes s (- #xaca #xac5))
    (let ((res (make-instance 'md-pattern
			    :position pos
			    :length pattern-length
			    :double-tempo double-tempo
			    :scale scale
			    :kit (or (gethash kit *kit-hash*) kit)
			    :accent accent-amount
			    :accent-track accent-track
			    :swing-track swing-track
			    :slide-track slide-track
			    :swing swing-amount
			    :tracks tracks)))
      (dolist (track (md-pattern-tracks res))
	(change-slot-values track 'pattern res))
      res)))

;;; song msg

(defun read-song-rows (s)
  (let ((res (list)))
    (loop for data = (read-sysex-bytes s 10)
	 for index from 0
	 for row = (make-instance 'md-row
				:index index
				:pattern (byte-to-row-pattern (elt data 0))
				:kit (or (gethash (elt data 1) *kit-hash*) (elt data 1))
				:loop-times (elt data 2)
				:jump-destination (elt data 3)
				:mutes (split-beat (subseq data 4 6))
				:tempo (make-long (subseq data 6 8))
				:start (elt data 8)
				:stop (elt data 9))
	 do (push row res)
	 when (eql (md-row-pattern row) :END)
	 do (return (nreverse res)))))

(defun read-song-msg (s)
  #+nil(format t "read-song-msg~%")
  (get-bytes s 2)
  (let* ((pos (get-byte s))
	 (name (make-c-string (get-bytes s 16)))
	 (rows (read-song-rows s)))
    (get-bytes s 5)
    (let ((song (make-instance 'md-song
			     :name name
			     :position pos
			     :rows rows)))
      (dolist (row rows)
	(change-slot-values row 'song song))
      song)))

(defun write-song-msg (song)
  (with-put-byte-to-list (s)
    (write-header s)
    (put-byte #x69 s)
    (put-bytes '(#x02 #x02) s)
    (start-checksum s)
    (put-byte (md-song-position song) s)
    (put-bytes (string-to-sysex (md-song-name song)) s)
    (loop for row in (md-song-rows song)
       do
	 (put-bytes
	  (data-to-sysex
	   (append (list (row-pattern-to-byte (md-row-pattern row))
			 (let ((kit (md-row-kit row)))
			   (if (numberp kit)
			       kit
			       (kit-position (md-row-kit row))))
			 (md-row-loop-times row)
			 (md-row-jump-destination row))
		   (beat-to-bytes (md-row-mutes row))
		   (short-to-bytes (md-row-tempo row))
		   (list (md-row-start row)
			 (md-row-stop row))))
	  s)
       until (eql (md-row-pattern row) :END))
	 
    (stop-checksum s)
    (put-bytes (get-checksum s) s)
    (put-bytes (short-to-sysex (- (get-length s) 7)) s)
    (put-byte #xf7 s)))


(defun write-0-string (string s)
  (write-string string s)
  (write-char #\Nul s))

(defun write-c6-file (outputfile files &key relative)
  (with-open-file (s outputfile :direction :output
		     :if-does-not-exist :create)
    (write-0-string "C6 Filelist" s)
    (write-0-string "0" s)
    (write-0-string "0" s)
    (dolist (file files)
      (if (or relative (equal (pathname-directory file) (pathname-directory outputfile)))
	  (write-0-string (format nil "00*~A.~A" (pathname-name file)
				  (pathname-type file)) s)
	  (write-0-string (format nil "00~A" (namestring file)) s)))))
