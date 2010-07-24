(in-package :md)

(declaim (optimize speed))

;;; XXX memory leak!!!

(defparameter *md-img-dir* "/Users/manuel/siff-svn/md-uw/pngs/")

(defvar *created-images* nil)

(defmacro my-create-image (&rest args)
  `(let ((img (create-image ,@args)))
     (push img *created-images*)
     img))

(defmacro my-create-image-from-file (&rest args)
  `(let ((img (create-image-from-file ,@args)))
     (push img *created-images*)
     img))

(defun destroy-created-images ()
  (dolist (img *created-images*)
    (destroy-image img))
  (format t "destroyed ~A images~%" (length *created-images*))
  (setf *created-images* nil))
  
(defmacro with-destroying-images (() &body body)
  `(let ((*created-images* (list)))
     (unwind-protect (progn ,@body)
       (destroy-created-images))))
     

(defun ram-test ()
  (dotimes (i 100)
    (with-destroying-images ()
      (dotimes (j 1000)
	(my-create-image 1000 1000 t)))))


(defun md-img-file (name)
  (make-pathname :name name :type "png"
		 :defaults *md-img-dir*))

(defparameter *md-knob-pos*
  '((0 4) (5 7) (8 12) (13 14) (15 15) (16 22) (23 25) (26 30)
    (31 32) (33 33) (34 40) (41 43) (44 47) (48 50) (51 51)
    (52 58) (59 61) (62 65) (66 68) (69 75) (76 76) (77 79)
    (80 83) (84 86) (87 93) (94 94) (95 96) (97 101)
    (102 104) (105 111) (112 112) (113 114) (115 119) (120 122)
    (123 127)))

(defparameter *md-img-hash* (make-hash-table :test #'equal))

(defun md-image (name)
  (let ((res (gethash name *md-img-hash*)))
    (if res
	res
	(setf (gethash name *md-img-hash*)
	      (create-image-from-file (md-img-file name))))))

(defun md-knob (position)
  (when (> 0 position)
    (setf position (+ position 64)))
  (loop for (start end) in *md-knob-pos*
     when (and (>= position start) (<= position end))
     do (return
	  (if (= start end)
	      (md-image (format nil "new-md-knob-~A" start))
	      (md-image (format nil "new-md-knob-~a-~A" start end))))))

(defun convert-knobs ()
  (loop for (start end) in *md-knob-pos*
       do
       (let* ((filename (if (= start end)
			    (format nil "md-knob-~A" start)
			    (format nil "md-knob-~A-~A" start end)))
	      (file (my-create-image-from-file (md-img-file filename)))
	      (destfilename (md-img-file (format nil "new-~A" filename))))
	 
	 (let ((black (allocate-color 0 0 0 :image file))
	       (white (allocate-color 255 255 255 :image file)))
	   (set-pixel 9 2 :color white :image file)
	   (set-pixel 9 3 :color black :image file)
	   (set-pixel 4 17 :color white :image file)
	   (set-pixel 14 17 :color white :image file)
	   (set-pixel 4 15 :color black :image file)
	   (set-pixel 14 15 :color black :image file))
	 (copy-image file file 0 2 0 0 19 19)
	 (write-image-to-file destfilename :if-exists :overwrite :image file))))

;; letter 4 x 5

(defparameter *md-font-file* (md-img-file "md-font2"))
;;; utf fuckup
(defparameter *md-alphabet*
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S
 #\T #\U #\V #\W #\X #\Y #\Z #\LATIN_CAPITAL_LETTER_A_WITH_DIAERESIS
 #\LATIN_CAPITAL_LETTER_A_WITH_ACUTE #\LATIN_CAPITAL_LETTER_O_WITH_DIAERESIS
 #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\/ #\( #\) #\. #\! #\? #\=
 #\> #\: #\# #\< #\* #\| #\' #\&))

(defparameter *font-img* (create-image-from-file *md-font-file*))

(defun md-text (text &key destination (dest-x 0) (dest-y 0) invert (scale 1))
  (let ((len (length text)))
    (unless destination
      (setf destination (my-create-image (* scale len 4) (* scale 5) t))
      (let ((white (allocate-color 255 255 255 :image destination)))
	(draw-rectangle* 0 0 (image-width destination) (image-height destination) :filled t
			 :color white :image destination)))
    (loop with font-img = *font-img*
       for char in (coerce (string-upcase text) 'list)
       for i from 0
       for pos = (position char *md-alphabet*)
       when pos
       do (when invert
	    (invert-img font-img))
	 (copy-image font-img destination (* pos 4) 0 (+ dest-x (* i scale 4)) dest-y
		     4 5 :resize t :dest-width (* scale 4) :dest-height (* scale 5)))
    destination))

(defun center-x (word len)
  (round (/ (- len (* (length word) 4)) 2)))

(defun invert-img (img &key (x 0) (y 0) width height)
  (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((*default-image* img))
    (unless width (setf width (- (image-width img) x)))
    (unless height (setf height (- (image-height img) y)))
    (let ((white (allocate-color 255 255 255 :image img))
	  (black (allocate-color 0 0 0 :image img)))
      (do-rows (y2)
	(when (and (>= y2 y) (< y2 (+ y height)))
	  (do-pixels-in-row (x2)
	    (when (and (>= x2 x) (< x2 (+ x width)))
	      (if (= black (raw-pixel))
		  (setf (raw-pixel) white)
		  (setf (raw-pixel) black)))))))))

(defparameter *params-img* (create-image-from-file (md-img-file "params")))

(defun md-params (params)
  (let ((img (clone-image *params-img*)))
    (loop for (name val) in params
       for i from 0
       for x = (if (< i 4) (* i 20) (* (- i 4) 20))
       for y = (if (< i 4) 0 32)
       for val-string = (princ-to-string val)
       do (progn
	    (md-text name :destination img
		     :dest-x (center-x name 20) :dest-y (+ y 3))
	    (copy-image (md-knob val) img 0 0
			(+ x 1) (+ y 11) 19 21)
	    (md-text val-string :destination img
		     :dest-x (center-x val-string 20) :dest-y (+ y 26))))
;    (invert-img img :x 2 :y 2 :height 7 :width 17)
    img))

(defun machine-name-string (machine)
  (coerce (subst #\> #\- (coerce (symbol-name
				  (machine-name (machine-model machine))) 'list))
	  'string))

(defun quotient (num divisor)
  (floor (/ num divisor)))

(defparameter *md-machine-name-img* (create-image-from-file (md-img-file "md-machine-name")))

(defparameter *md-lfo-img* (create-image-from-file (md-img-file "md-lfo")))

(defgeneric md-make-image (obj))

(defun md-machine-img (machine lfo track kit)
  (let* ((img (my-create-image 415 65 t))
	 (m-img *md-machine-name-img*)
	 (p-img *params-img*)
	 (kittext (format nil "KIT:~A" (kit-position kit)))
	 (model (machine-model machine))
	 (name (machine-name-string machine))
	 (track-name (symbol-name (track-name track)))
	 (lev-height (round (/ (machine-level machine) (/ 127 19))))
	 (black (allocate-color 0 0 0 :image img))
	 (white (allocate-color 255 255 255 :image img)))

    (draw-line 0 64 414 64 :color black :image img)

    (copy-image m-img img 0 0 0 0 48 64)
    (copy-image p-img img 1 0 48 0 80 64)
    (copy-image p-img img 1 0 128 0 80 64)
    (copy-image p-img img 1 0 208 0 80 64)
    (draw-rectangle* 37 (- 29 lev-height) 40 29 :image img :filled t :color black)
    (md-text kittext :destination img :dest-x (center-x kittext 48) :dest-y 34)
    (md-text (kit-name kit) :destination img :dest-x (center-x (kit-name kit) 48)
	     :dest-y 40)
    (md-text track-name :destination img :dest-x (center-x track-name 48) :dest-y 48)
    (md-text name :destination img :dest-x (center-x name 48) :dest-y 58)
    (invert-img img :x 2 :y 32 :width 44 :height 14)
    (invert-img img :x 1 :y 55 :width 46 :height 8)

    (case (machine-name (machine-model machine))
      (:CTR-8P
       (loop for i from 0 below 8
	    for x = (+ 48 (* 20 (mod i 4)))
	    for y = (if (>= i 4) 32 0)

	  for x2 = (+ 48 80 (* 40 (mod i 2)) (if (>= i 4) 80 0))
	  for y2 = (if (>= (mod i 4) 2) 32 0)
	    
	  for track = (machine-param-value machine (+ 8 (* i 2)))
	  for param = (machine-param-value machine (+ 8 (* i 2) 1))
	  for value = (machine-param-value machine i)
	  for destmachine = (elt (kit-machines kit) track)
	    
	  for trname = (format nil "TR~A:" (1+ track))
	  for trname2 = (format nil "TK~A" (1+ track))
	  for param-name = (model-param-name (machine-model destmachine) param)
	  for param-str  = (if (null param-name)
			       (format nil "(~A)" (1+ param))
			       (symbol-name param-name))
	  for pname = (format nil "P~A" (1+ i))
	  for value-str = (princ-to-string value)
	    
	  do (md-text trname :destination img :dest-x (+ x 1 (center-x trname 20))
		      :dest-y (+ y 12))
	    (md-text pname :destination img :dest-x (+ x 1 (center-x pname 20)) :dest-y (+ y 3))
	    (md-text param-str :destination img :dest-x (+ x 1 (center-x param-str 20))
		     :dest-y (+ y 19))
	    (md-text value-str :destination img :dest-x (+ x 1 (center-x value-str 20))
		     :dest-y (+ y 26))

	    (md-text pname :destination img :dest-x (+ x2 1 (center-x pname 20)) :dest-y (+ y2 3))
	    (md-text "TRK:" :destination img :dest-x (+ x2 1 (center-x "TRK:" 20))
		     :dest-y (+ y2 14))
	    (md-text trname2 :destination img :dest-x (+ x2 1 (center-x trname 20))
		     :dest-y (+ y2 23))

	    (md-text pname :destination img :dest-x (+ x2 1 20 (center-x pname 20))
		     :dest-y (+ y2 3))
	    (md-text "PAR:" :destination img :dest-x (+ x2 1 20 (center-x "PAR:" 20))
		     :dest-y (+ y2 14))
	    (md-text param-str :destination img :dest-x (+ 1 x2 20 (center-x param-str 20))
		     :dest-y (+ y2 23)))
       (copy-image *md-lfo-img* img 0 0 287 0 128 64))

       ((:MID-01 :MID-02 :MID-03 :MID-04 :MID-05 :MID-06
		:MID-07 :MID-08 :MID-09 :MID-10 :MID-11
		:MID-12 :MID-13 :MID-14 :MID-15 :MID-16)
	(loop for i from 0 below 24
	     for name = (param-name model i)
	     for namestr = (symbol-name name)
	     for x = (+ 48 (* 80 (quotient i 8)) (* 20 (mod i 4)))
	     for y = (if (>= (mod i 8) 4) 32 0)
	     do (md-text namestr :destination img :dest-x (+ x (center-x namestr 20))
							     :dest-y (+ y 3)))

	;; note fields
	(let* ((n1 (md-note-name (machine-param-value machine 0)))
	       (n2 (machine-param-value machine 1))
	       (n2-str (if (= n2 64) "OFF" (princ-to-string (+ -64 n2))))
	       (n3 (machine-param-value machine 2))
	       (n3-str (if (= n3 64) "OFF" (princ-to-string (+ -64 n3)))))
	  (md-text n1 :destination img :dest-x (+ 48 (center-x n1 20))
		   :dest-y 18)
	  (md-text n2-str :destination img :dest-x (+ 69 (center-x n2-str 20))
		   :dest-y 18)
	  (md-text n3-str :destination img :dest-x (+ 89 (center-x n3-str 20))
		   :dest-y 18))

	;; note len
	(let ((len-str (princ-to-string (machine-param-value machine 3)))
	      (black (allocate-color 0 0 0 :image img)))
	  (md-text len-str :destination img :dest-x (+ 109 (center-x len-str 20))
		   :dest-y 15)
	  (md-text "32" :destination img :Dest-x (+ 109 (center-x "32" 20))
		   :dest-y 23)
	  (draw-line 111 21 124 21 :color black :image img))

	;; numeric values
	(loop for i in '(4 5 6 7 9 11 13 15 17 19 20 21 22 23)
	   for val = (machine-param-value machine i)
	   for x = (+ 48 (* 80 (quotient i 8)) (* 20 (mod i 4)))
	   for y = (if (>= (mod i 8) 4) 32 0)
	   for val-string = (princ-to-string val)
	   do (copy-image (md-knob val) img 0 0 (+ x 1) (+ y 11) 18 20)
	     (md-text val-string :destination img
		      :dest-x (+ 1 x (center-x val-string 20)) :dest-y (+ y 26)))

	(loop for i in '(8 10 12 14 16 18)
	     for val = (machine-param-value machine i)
	     for x = (+ 48 (* 80 (quotient i 8)) (* 20 (mod i 4)))
	     for y = (if (>= (mod i 8) 4) 32 0)
	     for val-string = (case val
				(0 "OFF")
				;;; XXX false
				(t (princ-to-string val)))
	     do (md-text val-string :destination img
			 :dest-x (+ 1 x (center-x val-string 20))
			 :dest-y (+ y 19)))
	     

	(copy-image (md-lfo lfo machine track) img 0 0 287 0 128 64))
		
      (t (loop for val in (machine-params machine)
	    for i from 0
	    for name = (param-name model i)
	    for namestr = (if (symbolp name) (symbol-name name) "")
	    for x = (+ 48 (* 80 (quotient i 8)) (* 20 (mod i 4)))
	    for y = (if (>= (mod i 8) 4) 32 0)
	    for val-string = (princ-to-string val)
	    when (symbolp name)
	    do (progn
		 (md-text namestr :destination img :dest-x (+ x (center-x namestr 20))
			  :dest-y (+ y 3))
		 (when (eql (machine-name (machine-model machine)) :CTR-AL)
		   (invert-img img :x (+ x 1) :y (+ y 2)
			       :width 17 :height 7))
		 (copy-image (md-knob val) img 0 0 (+ x 1) (+ y 11) 18 20)
		 (md-text val-string :destination img
			  :dest-x (+ 1 x (center-x val-string 20)) :dest-y (+ y 26))))
	 (copy-image (md-lfo lfo machine track) img 0 0 287 0 128 64)
	 ))
	 
    img))

(defmethod md-make-image ((machine machine))
  (md-machine-img machine (machine-lfo machine) (machine-index machine) (machine-kit machine)))

(defun params-image (names-values)
  (let* ((img (my-create-image (image-width *params-img*)
			      (1+ (image-height *params-img*))
			      t))
	 (black (allocate-color 0 0 0 :image img)))
    (copy-image *Params-img* img 0 0 0 0 (image-width *params-img*)
		(image-height *params-img*))
    (draw-line 0 (1- (image-height img)) (image-width img) (1- (image-height img))
	       :color black :image img)
    (draw-line 0 0 0 (1- (image-height img))
	       :color black :image img)
    (loop for (name val) in names-values
	 for i from 0
       for namestr = (if (symbolp name) (symbol-name name) "")
       for x = (+ (* 80 (quotient i 8)) (* 20 (mod i 4)))
       for y = (if (>= (mod i 8) 4) 32 0)
       for val-string = (princ-to-string val)
       when (and name (symbolp name))
       do (progn
	    (md-text namestr :destination img :dest-x (+ x (center-x namestr 20))
		     :dest-y (+ y 3))
	    (copy-image (md-knob val) img 0 0 (+ x 1) (+ y 11) 18 20)
	    (md-text val-string :destination img
		     :dest-x (+ 1 x (center-x val-string 20)) :dest-y (+ y 26))))
    img))


;;; DOC

(defun doc-generate ()
  (progn
    (write-image-to-file "/users/manuel/crunchy-snare1.png"
			 :if-exists :overwrite
			 :image (params-image '((:PTCH 13) (:DEC 110) (:BUMP 44) (:BENV 0)
						(:SNAP 60) (:TONE 127) (:TUNE 127) (:CLIP 57))))
    
    (write-image-to-file "/users/manuel/crunchy-snare2.png"
			 :if-exists :overwrite
			 :Image (params-image '((:AMD 0) (:AMF 0) (:EQF 24) (:EQG 63)
						(:FTLF 39) (:FLTW 49) (:FTLQ 109) (:SRR 11))))
    (write-image-to-file "/users/manuel/crunchy-snare3.png"
			 :if-exists :overwrite
			 :image (params-image '((:DIST 127) (:VOL 127) (:PAN 6) (:DEL 0)
						(:REV 1) (:LFOS 108) (:LFOD 66) (:LFOM 64))))
    (write-image-to-file "/users/manuel/crunch-snare-lfo.png"
			 :if-exists :overwrite
			 :image (md-lfo-blabla "TRX-SN" "02-SN" "PTCH" 0 4 "FREE" 108 68 64))
    
    (write-image-to-file "/users/manuel/tune-e12.png"
			 :if-exists :overwrite
			 :image (params-image '((:PTCH -37) (:DEC 127) (:HP 1) (:RATL 127)
						(:STRT 41) (:RTRG 44) (:RTIM 45) (:BEND -16))))
    (write-image-to-file "/users/manuel/violin-hh.png"
			 :if-exists :overwrite
			 :image (params-image
				 '((:PTCH 70) (:DEC 96) (:TREM 101) (:TFRQ 64)
				   (:MOD 96) (:MFRQ 96) (:MDEC 127) (:FB 19))))
    (write-image-to-file "/users/manuel/violin-hh2.png"
			 :if-exists :overwrite
			 :image (params-image
				 '(nil nil nil nil (:FLTF 64) (:FTLW 71) nil nil)))
    (write-image-to-file "/users/manuel/violin-hh3.png"
			 :if-exists :overwrite
			 :image (params-image
				 '((:DIST 64) nil nil nil
				   (:REV 127) (:LFOS 34) (:LFOD 54) (:LFOM 127))))
    (write-image-to-file "/users/manuel/violin-lfo.png"
			 :if-exists :overwrite
			 :image (md-lfo-blabla "EFM-HH" "HH" "FLTW" 0 3 "TRIG" 108 68 64))
    
    
    
    ))

(defgeneric md-image-file (obj filename))
(defmethod md-image-file (obj filename)
  (write-image-to-file filename
		       :if-exists :overwrite
		       :image (md-make-image obj)))

(defun clone-image (img)
  (let ((image (my-create-image (image-width img) (image-height img) t)))
    (copy-image img image 0 0 0 0 (image-width img) (image-height img))
    image))

(defun md-note-name (note)
  (let ((octave (1- (octave-number note)))
	(name (coerce (symbol-name (note-name note)) 'list)))
    (setf name (subst #\# #\S name))
    (if (member #\# name)
	(format nil "~A~A" (coerce name 'string) octave)

	(format nil "~A ~A" (coerce name 'string) octave))))

(defun md-lfo-blabla (name dest-name param-name shape1 shape2 type-name speed depth mix)
  (let* ((img (my-create-image (image-width *md-lfo-img*)
			       (1+ (image-height *md-lfo-img*))))
	 (white (allocate-color 255 255 255 :image img))
	 (black (allocate-color 0 0 0 :image img)))
    (copy-image *md-lfo-img* img 0 0 0 0 (image-width *md-lfo-img*)
		(image-height *md-lfo-img*))
    (draw-line 0 (1- (image-height img)) (image-width img) (1- (image-height img))
	       :color black :image img)
    (invert-img img :x 0 :y 3 :width 128 :height 6)
    (md-text name :destination img :dest-x (+ 3 (center-x name 128)) :dest-y 3)
    (invert-img img :x 0 :y 3 :width 128 :height 6)
    (md-text dest-name :destination img :dest-x (+ 11 (center-x dest-name 22)) :dest-y 25)
    (md-text param-name :destination img :dest-x (+ 35 (center-x param-name 22)) :dest-y 25)
    (copy-image (md-image (format nil "md-lfo1-~A" shape1))
		img 0 0 61 23 18 10)
    (copy-image (md-image (format nil "md-lfo2-~A" shape2))
		img 0 0 85 23 18 10)
    (md-text type-name :destination img :dest-x (+ 11 (center-x type-name 22)) :dest-y 47)

    (copy-image (md-knob speed)
		img 4 3 40 45 11 11)
    (set-pixel 40 55 :color white :image img)
    (set-pixel 50 55 :color white :image img)

    (copy-image (md-knob depth)
		img 4 3 64 45 11 11)
    (set-pixel 64 55 :color white :image img)
    (set-pixel 74 55 :color white :image img)

    (copy-image (md-knob mix)
		img 4 3 88 45 11 11)
    (set-pixel 88 55 :color white :image img)
    (set-pixel 98 55 :color white :image img)
    
    img))

(defun md-lfo (lfo machine track)
  (let* ((img (clone-image *md-lfo-img*))
	 (name (format nil "LFO ~A" (track-name track)))
	 (dest-name (symbol-name (track-name (lfo-destination lfo))))
	 (param (param-name (machine-model machine) (lfo-param lfo)))
	 (param-name (if (symbolp param) (symbol-name param) ""))
	 (type-name (symbol-name (lfo-type-name (lfo-type lfo))))
	 (white (allocate-color 255 255 255 :image img)))
    (invert-img img :x 0 :y 3 :width 128 :height 6)
    (md-text name :destination img :dest-x (+ 3 (center-x name 128)) :dest-y 3)
    (invert-img img :x 0 :y 3 :width 128 :height 6)
    (md-text dest-name :destination img :dest-x (+ 11 (center-x dest-name 22)) :dest-y 25)
    (md-text param-name :destination img :dest-x (+ 35 (center-x param-name 22)) :dest-y 25)
    (copy-image (md-image (format nil "md-lfo1-~A" (lfo-shape1 lfo)))
		img 0 0 61 23 18 10)
    (copy-image (md-image (format nil "md-lfo2-~A" (lfo-shape2 lfo)))
		img 0 0 85 23 18 10)
    (md-text type-name :destination img :dest-x (+ 11 (center-x type-name 22)) :dest-y 47)

    (copy-image (md-knob (elt (machine-params machine) 21))
		img 4 3 40 45 11 11)
    (set-pixel 40 55 :color white :image img)
    (set-pixel 50 55 :color white :image img)

    (copy-image (md-knob (elt (machine-params machine) 22))
		img 4 3 64 45 11 11)
    (set-pixel 64 55 :color white :image img)
    (set-pixel 74 55 :color white :image img)

    (copy-image (md-knob (elt (machine-params machine) 23))
		img 4 3 88 45 11 11)
    (set-pixel 88 55 :color white :image img)
    (set-pixel 98 55 :color white :image img)
    
    img))



(defmethod md-make-image ((kit kit))
  (md-kit-img kit))
(defun md-kit-img (kit)
  (let ((img (my-create-image 415 (* 16 64) t)))
    (loop for i from 0
	 for machine in (kit-machines kit)
	 for lfo in (kit-lfos kit)
	 do (copy-image (md-machine-img machine lfo i kit)
			img 0 0 0 (* i 64) 415 64))
    img))

(defun mute-pattern-string (mutes)
  (coerce
   (loop for mute in mutes
	for i from 0
      collect (if (= mute 0)
		  (if (= (mod i 4) 0)
		      #\* #\&)
		  #\-))
   'string))

;;; trigs and mutes for kit track XXX

(defun max-track-plock-len (track)
  (reduce #'max (loop for i from 0 below 32
			 collect (length (track-plocks-on-step track i))) :initial-value 0))

(defun pattern-track-empty-p (track)
  (every #'(lambda (x) (= x 0)) (md-pattern-track-hits track)))

(defun md-pattern-img (pattern)
  (let* ((images (loop for idx from 0 below 16
		    for track in (md-pattern-tracks pattern)
		      unless (pattern-track-empty-p track)
		    collect (md-pattern-track-img pattern track idx)))
	 (len (md-pattern-length pattern))
	 (height (reduce #'+ (mapcar #'image-height images) :initial-value 0))
	 (image (my-create-image (+ 80 (* 24 len) 4) height t)))
    (loop for img in images
	 for i from 0
	 with y = 0
	 do (copy-image img image 0 0 0 y (image-width img) (image-height img))
	 (incf y (image-height img)))
    image))

(defmethod md-make-image ((pattern md-pattern))
  (md-pattern-img pattern))

(defun test-pattern (pattern)
  (write-image-to-file "/users/manuel/pattern.png" :if-exists :overwrite
		       :image (md-pattern-img pattern)))
		     
(defun md-pattern-track-img (pattern track idx)
  (let* ((len (md-pattern-length pattern))
	 (plock-len (max-track-plock-len track))
	 (image (my-create-image (+ 80 (* 24 len) 4) (+ 32 #+nil(* plock-len 32)) t))
	 (black (allocate-color 0 0 0 :image image))
	 (white (allocate-color 255 255 255 :image image))
	 (kit (md-pattern-kit pattern))
	 (kit-track (elt (kit-machines kit) idx))
	 (model (machine-model kit-track)))
    (draw-rectangle* 0 0 (+ 80 (* 24 len) 4)
		     (+ (* plock-len 32) 32) :color white :image image :filled t)
    (md-text (format nil "TRK ~A:" idx) :destination image :dest-x 3 :dest-y 1 :scale 2)
    (md-text (machine-name-string kit-track) :destination image :dest-x 3 :dest-y 13 :scale 2)
    (with-slots (hits accent swing slide plock-pattern plocks) track
      (loop for step from 0 below len
	   for hit in hits
	   for acc in accent
	   for sw in swing
	   for sl in slide
	   for plocks = (track-plocks-on-step track step)
	   for x = (+ 1 80 (* 24 step))
	   for y = 2
	 do (if (= hit 1)
		(progn
		  (draw-rectangle* (+ 6 x) (+ 6 y) (+ 16 x) (+ 15 y) :color black :image image)
		  (draw-rectangle* (+ 5 x) (+ 5 y) (+ 17 x) (+ 16 y)
				   :filled (= (mod step 4) 0)
				   :color black :image image))
		(draw-rectangle* (+ 5 x) (+ 9 y) (+ 17 x) (+ 10 y)
				 :color black :filled t :image image))
	   #+nil
	   (when (not (null plocks))
	     #+nil(draw-rectangle* (1+ x) (+ 21 y) (+ 19 x) (+ 21 y) :color black :image image))
	   #+nil
	   (loop for plock in plocks
	      for idx from 0
	      do (draw-plock pattern kit-track plock step model idx image x (+ 20 y (* idx 32))))))
    
    image))

(defmethod md-make-image ((track md-pattern-track))
  (md-pattern-track-img (md-pattern-track-pattern track) track (md-pattern-track-index track)))

(defun draw-plock (pattern machine plock step model idx image x y)
  (let* ((param (plock-param plock))
	 (vals (plock-vals plock))
	 (value (elt vals step))
	 (black (allocate-color 0 0 0 :image image)))
    (draw-rectangle* x y (+ x 24) (+ y 30) :color black :image image)
    (incf x 2)
    (case (machine-name model)
      (:CTR-8P
       (let* ((track (machine-param-value machine (+ 8 (* param 2))))
	      (param2 (machine-param-value machine (+ 8 (* param 2) 1)))
	      (destmachine (elt (kit-machines (md-pattern-kit pattern)) track))
	      (trname (format nil "TR~A:" (1+ track)))
	      (trname2 (format nil "TK~A" (1+ track)))
	      (param-name (model-param-name (machine-model destmachine) param2))
	      (param-str (if (null param-name)
			     (format nil "(~A)" (1+ param2))
			     (symbol-name param-name)))
	      (pname (format nil "P~A" (1+ param2)))
	      (value-str (princ-to-string value)))
	 (cond
	   ((< param 8)
	    (md-text pname :destination image  :dest-x (+ x 1 (center-x pname 20))
		     :dest-y (+ y 3))
	    (md-text trname :destination image :dest-x (+ x 1 (center-x trname 20))
		     :dest-y (+ y 10))
	    (md-text param-str :destination image :dest-x (+ x 1 (center-x param-str 20))
		     :dest-y (+ y 17))
	    (md-text value-str :destination image :dest-x (+ x 1 (center-x value-str 20))
		     :dest-y (+ y 24)))
	   
	   ((and (>= param 8)
		 (= (mod param 2) 0))
	    (md-text pname :destination image :dest-x (+ x 1 (center-x pname 20))
		     :dest-y (+ y 3))
	    (md-text "TRK:" :destination image :dest-x (+ x 1 (center-x "TRK:" 20))
		     :dest-y (+ y 12))
	    (md-text trname2 :destination image :dest-x (+ x 1 (center-x trname2 20))
		     :dest-y (+ y 24)))
	   
	   ((and (>= param 8)
		 (= (mod param 2) 1))
	    (md-text pname :destination image :dest-x (+ x 1 (center-x pname 20))
		     :dest-y (+ y 3))
	    (md-text "PAR:" :destination image :dest-x (+ x 1 (center-x "PAR:" 20))
		     :dest-y (+ y 12))
	    (md-text param-str :destination image :dest-x (+ x 1 (center-x param-str 20))
		     :dest-y (+ y 24))))))
      
      ((:MID-01 :MID-02 :MID-03 :MID-04 :MID-05 :MID-06
		:MID-07 :MID-08 :MID-09 :MID-10 :MID-11
		:MID-12 :MID-13 :MID-14 :MID-15 :MID-16)
       (let* ((param-name (param-name model param))
	      (param-str (symbol-name param-name)))
	 (md-text param-str :destination image :dest-x (+ x (center-x param-str 20))
		  :dest-y (+ y 3))

	 (case param
	   (0 (let ((n1 (md-note-name value)))
		(md-text n1 :destination image :dest-x (+ x (center-x n1 20))
			 :dest-y 18)))
	   ((1 2)
	    (let* ((note value)
		   (str (if (= note 64) "OFF" (princ-to-string (+ -64 note)))))
	      (md-text str :destination image :dest-x (+ x (center-x str 20))
		       :dest-y 18)))

	   (3 (let ((len-str (princ-to-string value)))
		(md-text len-str :destination image :dest-x (+ x (center-x len-str 20))
			 :dest-y 15)
		(md-text "32" :destination image :dest-x (+ x (center-x "32" 20))
			 :dest-y 23)
		(draw-line (+ x 2) 21 (+ x 15) 21 :color black :image image)))

	   ((4 5 6 7 9 11 13 15 17 19 20 21 22 23)
	    (let ((val-string (princ-to-string value)))
	      (copy-image (md-knob value) image 0 0 x (+ y 9) 18 20)
	      (md-text val-string :destination image :dest-x (+ x (center-x val-string 20))
		       :dest-y (+ y 24))))

	   ((8 10 12 14 16 18)
	    (let ((val-string (case value (0 "OFF")
				    (t (princ-to-string value)))))
	      (md-text val-string :destination image
		       :Dest-x (+ 1 x (center-x val-string 20))
		       :dest-y (+ y 19)))))))

      (t
       (let* ((name (param-name model param))
	      (namestr (if (symbolp name) (symbol-name name) ""))
	      (val-string (princ-to-string value)))
	 (md-text namestr :destination image :dest-x (+ x (center-x namestr 20))
		  :dest-y (+ y 3))
	 (copy-image (md-knob value) image 0 0 x (+ y 9) 18 20)
	 (md-text val-string :destination image
		  :dest-x (+ x (center-x val-string 20)) :dest-y (+ y 24)))))))
  
(defun test-track (track)
  (write-image-to-file "/users/manuel/track.png" :if-exists :overwrite
		       :image (md-pattern-track-img *pat* track 0)))

(defun md-song-hdr ()
  (let* ((image (my-create-image 160 8 t))
	 (white (allocate-color 255 255 255 :image image))
	 (black (allocate-color 0 0 0 :image image)))
    (draw-rectangle* 0 0 160 10 :filled t :color white :image image)
    (md-text " ROW|PAT|REP|OF|LN|BPM|MUTES" :destination image :dest-x  0 :dest-y 1)
    (loop for x in '(17 33 49 61 73 89)
	 do (set-pixel x 6 :color black :image image))
    (draw-line 0 7 159 7 :color black :image image)
    image))

(defun md-row-img (row patnum)
  (let* ((image (my-create-image 160 7 t))
	 (white (allocate-color 255 255 255 :image image))
	 (black (allocate-color 0 0 0 :image image)))
    (draw-rectangle* 0 0 160 10 :filled t :color white :image image)
    ;;; XXX end handling, infinity symbol
    (case (md-row-pattern row)
      (:jump
       (md-text (with-slots (loop-times jump-destination) row
		  (if (> loop-times 0)
		      (format nil " ~3,'0D|LOOP: ~3,'0D/~2,'0D" patnum jump-destination loop-times)
		      (format nil " ~3,'0D|JUMP: ~3,'0D" patnum jump-destination)))
		:destination image :dest-x 0 :dest-y 1)
       (set-pixel 17 0 :color black :image image)
       (set-pixel 17 6 :color black :image image))
      (:end
       (md-text (format nil " ~3,'0D|END" patnum) :destination image :Dest-x 0 :dest-y 1)
       (set-pixel 17 0 :color black :image image)
       (set-pixel 17 6 :color black :image image))
      (t
       (loop for x in '(17 33 49 73 89)
	  do (set-pixel x 0 :color black :image image)
	    (set-pixel x 6 :color black :image image))
       
       (md-text (with-slots (pattern loop-times tempo start stop mutes) row
		  (format nil
			  " ~3,'0D|~3,'0D|~3,'0D|~2,'0D'~2,'0D|~3,'0D|~A" patnum (pattern-name pattern)
			  loop-times start stop 120 (mute-pattern-string mutes)))
		:destination image :dest-x 0 :dest-y 1)))
    image))

(defmethod md-make-image ((row md-row))
  (md-row-img row (md-row-index row)))

(defun md-song-img (song)
  (let* ((rows (md-song-rows song))
	 (len (length rows)))
    (let ((image (my-create-image 160 (+ 8 (* len 7)))))
      (copy-image (md-song-hdr) image 0 0 0 0 160 8)
      (loop for pat from 0 below len
	   for row in rows
	   do (copy-image (md-row-img row pat) image 0 0 0 (+ 8 (* pat 7)) 160 7))
      image)))

(defmethod md-make-image ((song md-song))
  (md-song-img song))

(defun scale-image (img scale)
  (if (= scale 1)
      img
      (let ((image (my-create-image (* (image-width img) scale)
				    (* (image-height img) scale) t)))
	(copy-image img image 0 0 0 0 (image-width img) (image-height img) :resize t
		    :dest-width (image-width image)
		    :dest-height (image-height image))
	image)))

(defun test-row-hdr ()
  (write-image-to-file "/users/manuel/row-hdr.png" :if-exists :overwrite
		       :image (md-song-hdr)))

(defun test-row ()
  (write-image-to-file "/users/manuel/row.png" :if-exists :overwrite
		       :image (md-row-img *row-pat* 0)))

(defun test-song (song)
  (write-image-to-file "/users/manuel/song.png" :if-exists :overwrite
		       :image (md-song-img song)))

(defvar *kit*)
(defvar *lfo*)
(defvar *machine*)
(defvar *song*)
(defvar *row-pat*)
(defvar *track*)
(defvar *pat*)

(defun test-kit (kit)
  (write-image-to-file "/users/manuel/kit.png" :if-exists :overwrite
		      :image (md-kit-img kit)))

(defun test-lfo ()
  (write-image-to-file "/users/manuel/lfo.png" :if-exists :overwrite
		       :image (md-lfo *lfo* *machine* 0)))

(defun test-machine ()
  (write-image-to-file "/users/manuel/machine.png" :if-exists :overwrite
		       :image (md-machine-img *machine* *lfo* 0 *kit*)))

(defun test-params ()
  (write-image-to-file "/users/manuel/bla.png" :if-exists :overwrite :image (md-params '(("ptch" 4)))))

