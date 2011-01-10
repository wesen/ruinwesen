(in-package :md)

(defparameter *all-mapping*
  '(
    (C2 . C2) (CS2 . CS2) (D2 . D2) (EF2 . EF2) (E2 . E2) (F2 . F2) (FS2 . FS2)
    (G2 . G2) (AF2 . AF2) (A2 . A2) (BF2 . BF2) (B2 . B2)    
    (C3 . C3) (CS3 . CS3) (D3 . D3) (EF3 . EF3) (E3 . E3) (F3 . F3) (FS3 . FS3)
    (G3 . G3) (AF3 . AF3) (A3 . A3) (BF3 . BF3) (B3 . B3) (C4 . C4) (CS4 . CS4)
    (D4 . D4) (EF4 . EF4) (E4 . E4) (F4 . F4) (FS4 . FS4) (G4 . G4) (AF4 . AF4)
    (A4 . A4) (BF4 . BF4) (B4 . B4) (C5 . C5) (CS5 . CS5) (D5 . D5) (EF5 . EF5)
    (E5 . E5) (F5 . F5) (FS5 . FS5) (G5 . G5) (AF5 . AF5) (A5 . A5) (BF5 . BF5)
    (B5 . B5) (C6 . C6) (CS6 . CS6) (D6 . D6)))

(defparameter *test-mapping* '((c4 . c3)))

(defun convert-keynum (keynum mapping)
  (let ((newk (assoc (to-keyname keynum) mapping)))
    (if newk
	(cdr newk)
	nil)))

(defun convert-file (infile mapping)
  (when infile
    (loop for obj in (subobjects (import-events (namestring infile)))
       if (and (typep obj 'midi) (not (null (convert-keynum (cm::midi-keynum obj) mapping))))
       collect (new midi :time (object-time obj)
		    :duration (cm::midi-duration obj)
		    :keynum (convert-keynum (cm::midi-keynum obj) mapping)
		    :amplitude (cm::midi-amplitude obj)))))


(defun to-keyname (bla)
  (let* ((num (keynum bla))
	 (name (note-name num))
	 (octave (1- (octave-number num))))
    (intern (format nil "~A~A" name octave))))

(defun convert-files (outfile &rest infile-mappings)
  (events
   (append
    (list (new midi-time-signature :time 0 :numerator 4 :denominator 4 :clocks 36 :32nds 8))
    (list (cm::process cm::for i cm::in (sort (copy-tree
				   (loop for (infile mapping) on infile-mappings by #'cddr
				      nconc (convert-file infile mapping)))
				  #'<
				  :key #'object-time)
		       cm::output i)))
   (namestring outfile)))

(defparameter *d1-map*
  '((f4 . c2)
    (c4 . g2)
    (b4 . f2)
    (e4 . d2)
    (d4 . e3)))

(defparameter *drums1-map*
  '((c4 . a3)
    (d4 . b3)
    (g4 . c4)
    (f4 . d4)))


(defun convert-file-list (map1 map2 
			  files1 files2 &key (outname "out"))
  (let* ((first-file (find-if #'pathnamep files1))
	 (dir (pathname-directory first-file)))
    (loop for f1 in files1
       for f2 in files2
	 for i from 1
	 for outfile = (make-pathname :name (format nil "~A~A" outname i) :type "mid"
				      :directory dir)
	 do (format t "merging ~A and ~A to ~A~%" f1 f2 outfile)
	 do (convert-files outfile f1 map1 f2 map2))))

(defun convert-file-list-dir (map1 map2 dir files1 files2 &key (outname "out"))
  (convert-file-list map1 map2 (files-in-dir dir files1) (files-in-dir dir files2) :outname outname))

(defun files-in-dir (dir names)
  (mapcar #'(lambda (x) (when x (merge-pathnames dir x))) names))

#+nil
(convert-file-list-dir *d1-map* *drums1-map* "/Users/manuel/quixit-47-mids/"
		       '("d1.mid" "d2.mid" "d3.mid" "d4.mid" "d5.mid" "d6.mid" "d7.mid" "d8.mid"
			 "d9.mid")
		       '(nil nil "drums2.mid" "drums3.mid" "drums4.mid" "drums5.mid" "drums6.mid"
			 "drums7.mid" "drums8.mid"))

(defun merge-files (file1 file2)
  (convert-files file1 file1 nil file2 nil))

;; acid test
(defparameter *drums*
  '((b4 . f3)
    (c4 . d3)
    (e4 . e3)
    (d4 . b2)
    (g4 . a2)))

(defparameter *drums2*
  '((a4 . f3)
    (b4 . f3)
    (d4 . d2)
    (e4 . g3)))

#+nil
(convert-file-list-dir *drums* *drums2* "/Users/manuel/acid-test-mids/"
		       '("hh1.mid" "hh2.mid" "hh3.mid" "hh4.mid" "hh5.mid"
			 "hh6.mid" "hh7.mid" "hh8.mid" "hh9.mid" "hh10.mid"
			 "hh11.mid" "hh12.mid" "hh13.mid" "hh14.mid" "hh15.mid"
			 "hh16.mid" "hh17.mid" "hh18.mid")
		       '(nil nil "drum1.mid" "drum2.mid" "drum3.mid" "drum4.mid"
			 "drum5.mid" nil "drum6.mid" "drum7.mid" "drum8.mid" "drum9.mid"
			 "drum10.mid" nil "drum11.mid" "drum12.mid" nil "drum13.mid"))

#+nil
(merge-files "/Users/manuel/acid-test-mids/out1.mid" "/Users/manuel/acid-test-mids/md.mid")

(defparameter *lyon-drum1*
  '((c3 . c2)
    (e3 . d2)
    (c4 . a3)
    (gs3 . e2)
    (ds3 . a2)))

(defparameter *lyon-drum2*
  '((b4 . f3)
    (d3 . a2)))

(defparameter *lyon-drum3*
  '((b4 . b3)
    (c5 . c4)))

#+nil
(convert-file-list-dir *lyon-drum1* *lyon-drum2*
		       "/Users/manuel/detroit-lyon-mids/"
		       '("d1.mid" "d2.mid" "d3.mid" "d4.mid" "d5.mid" "d6.mid"
			 nil "d7.mid" "d8.mid" "d9.mid" "d10.mid" "d11.mid"
			 "d12.mid" "d13.mid")
		       '(nil "e1.mid" "e2.mid" "e3.mid" "e4.mid" "e5.mid"
			 nil "e6.mid" "e7.mid" "e8.mid" "e9.mid" nil "e10.mid" nil
			 "e11.mid")
		       :outname "temp")

#+nil
(convert-file-list-dir *all-mapping* *lyon-drum3*
		       "/Users/manuel/detroit-lyon-mids/"
		       '("temp1.mid" "temp2.mid" "temp3.mid" "temp4.mid" "temp5.mid" "temp6.mid"
			 "temp7.mid" "temp8.mid" "temp9.mid" "temp10.mid" "temp11.mid"
			 "temp12.mid" "temp13.mid" "temp14.mid")
		       '(nil "hh1.mid" "hh2.mid" "hh3.mid" "hh4.mid" "hh5.mid"
			 nil "hh6.mid" "hh7.mid" "hh8.mid" "hh9.mid" nil "hh10.mid"
			 "hh11.mid" "hh12.mid")
		       :outname "out")

(defparameter *plagne-map*
  '((d4 . e3)
    (b4 . f3)
    (a4 . g3)
    (c4 . d2)
    (g4 . a3)))

#+nil
(convert-file-list-dir *plagne-map* *all-mapping*
		       "/Users/manuel/laplagne-mids/"
		       '("c1.mid" "c2.mid" "c3.mid" "c4.mid" "c5.mid" "c6.mid"
			 "c7.mid" "c8.mid" "c9.mid" "c10.mid" "c11.mid")
		       '("d1.mid" "d2.mid" "d3.mid" "d4.mid" "d5.mid" "d6.mid"
			 "d7.mid" "d8.mid" "d9.mid" "d10.mid" "d11.mid"))

(defparameter *808-map*
  '((c4 . c2)
    (d4 . d2)
    (e4 . e2)
    (f4 . f3)
    (g4 . a2)
    (a4 . c3)
    (b4 . d3)
    (c5 . e3)))

(defparameter *bb-map*
  '((c4 . c4)
    (d4 . d4)
    (b4 . a3)
    (c5 . b3)))

#+nil
(convert-file-list-dir *808-map* *bb-map*
		       "/Users/manuel/thrombosis-mids/"
		       '("d1.mid" "d2.mid" "d3.mid" "d4.mid" "d5.mid" "d6.mid"
			 "d7.mid" "d8.mid" "d9.mid" "d10.mid" "d11.mid" "d12.mid"
			 "d13.mid" "d14.mid" nil)
		       '("bb1.mid" "bb2.mid" "bb3.mid" "bb4.mid" "bb5.mid" "bb6.mid"
			 nil "bb7.mid" "bb8.mid" "bb9.mid" "bb10.mid" "bb11.mid" "bb12.mid"
			 "bb13.mid" "bb14.mid"))
			 