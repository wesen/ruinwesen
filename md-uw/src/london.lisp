(in-package :md)

#|
hase:

MD> (renumber-patterns (group-with-name "hase2") :a01)
(#<MD-PATTERN POS: A01 {11E26381}> #<MD-PATTERN POS: A02 {11E26411}>
 #<MD-PATTERN POS: A03 {11E26391}> #<MD-PATTERN POS: A05 {11E26421}>
 #<MD-PATTERN POS: A07 {11E26351}> #<MD-PATTERN POS: A08 {11E26371}>1
 #<MD-PATTERN POS: A09 {11E26361}> #<MD-PATTERN POS: A10 {11E26401}>
 #<MD-PATTERN POS: A13 {11E263F1}> #<MD-PATTERN POS: B01 {11E263A1}>
 #<MD-PATTERN POS: B02 {11E263B1}> #<MD-PATTERN POS: B03 {11E263C1}>
 #<MD-PATTERN POS: B04 {11E263D1}> #<MD-PATTERN POS: B05 {11E263E1}>)
STYLE-WARNING: redefining RENUMBER-PATTERNS in DEFUN
MD> (renumber-patterns (group-with-name "hase2") :a01)
(#<MD-PATTERN POS: A01 {11E26381}> #<MD-PATTERN POS: A02 {11E26411}>
 #<MD-PATTERN POS: A03 {11E26391}> #<MD-PATTERN POS: A04 {11E26421}>
 #<MD-PATTERN POS: A05 {11E26351}> #<MD-PATTERN POS: A06 {11E26371}>
 #<MD-PATTERN POS: A07 {11E26361}> #<MD-PATTERN POS: A08 {11E26401}>
 #<MD-PATTERN POS: A09 {11E263F1}> #<MD-PATTERN POS: A10 {11E263A1}>
 #<MD-PATTERN POS: A11 {11E263B1}> #<MD-PATTERN POS: A12 {11E263C1}>
 #<MD-PATTERN POS: A13 {11E263D1}> #<MD-PATTERN POS: A14 {11E263E1}>)
MD>
|#

#|
alles2:

(#<KIT NAME: IDM2 ID: 30868 POS: 30> #<KIT NAME: IDM ID: 30901 POS: 27>
 #<KIT NAME: DUBSTEPELK ID: 30934 POS: 29> #<KIT NAME: Q49 ID: 30967 POS: 28>
 #<KIT NAME: EFM KIT ID: 31000 POS: 17> #<KIT NAME: ELECTRO2 ID: 31033 POS: 1>
 #<KIT NAME: ELECTRO ID: 31066 POS: 0> #<KIT NAME: IDM6 ID: 31099 POS: 34>
 #<KIT NAME: IDM4 ID: 31132 POS: 32> #<KIT NAME: IDM5 ID: 31165 POS: 33>
 #<MD-PATTERN POS: D10 {11EAF091}> #<MD-PATTERN POS: D08 {11EAF081}>
 #<MD-PATTERN POS: D06 {11EAF071}> #<MD-PATTERN POS: D04 {11EAF061}>
 #<MD-PATTERN POS: D03 {11EAF051}> #<MD-PATTERN POS: D02 {11EAF041}>
 #<MD-PATTERN POS: A11 {11EAF031}> #<MD-PATTERN POS: A09 {11EAF021}>
 #<MD-PATTERN POS: A05 {11EAF011}> #<MD-PATTERN POS: A04 {11EAF001}>
 #<MD-PATTERN POS: A03 {11EAEFF1}> #<MD-PATTERN POS: A02 {11EAEFE1}>
 #<MD-PATTERN POS: A01 {11EAEFD1}> #<MD-PATTERN POS: G03 {11EAEFC1}>
 #<MD-PATTERN POS: G02 {11EAEFB1}> #<MD-PATTERN POS: G01 {11EAEFA1}>)
MD> (make-object 'group :name "alles2" :objects (subseq (group-objects *exported*) 21 47))

MD> (renumber-patterns (group-with-name "alles2") :b01)
(#<MD-PATTERN POS: B01 {11EAEFD1}> #<MD-PATTERN POS: B02 {11EAEFE1}>
 #<MD-PATTERN POS: B03 {11EAEFF1}> #<MD-PATTERN POS: B04 {11EAF001}>
 #<MD-PATTERN POS: B05 {11EAF011}> #<MD-PATTERN POS: B06 {11EAF021}>
 #<MD-PATTERN POS: B07 {11EAF031}> #<MD-PATTERN POS: B08 {11EAF041}>
 #<MD-PATTERN POS: B09 {11EAF051}> #<MD-PATTERN POS: B10 {11EAF061}>
 #<MD-PATTERN POS: B11 {11EAF071}> #<MD-PATTERN POS: B12 {11EAF081}>
 #<MD-PATTERN POS: B13 {11EAF091}> #<MD-PATTERN POS: B14 {11EAEFA1}>
 #<MD-PATTERN POS: B15 {11EAEFB1}> #<MD-PATTERN POS: B16 {11EAEFC1}>)
|#

#|
solo2:

renumbering A01 to C01
renumbering A02 to C02
renumbering A03 to C03
renumbering A05 to C04
renumbering A09 to C05
renumbering A13 to C06
renumbering B01 to C07
renumbering B02 to C08
renumbering B05 to C09
renumbering B09 to C10
renumbering B13 to C11
renumbering C01 to C12
renumbering C05 to C13
renumbering C09 to C14
renumbering C13 to C15
renumbering D01 to C16
renumbering D05 to D01
renumbering D09 to D02
renumbering D13 to D03
renumbering E01 to D04
renumbering F01 to D05
renumbering F02 to D06
|#

#|
short2

renumbering C05 to F01
renumbering C06 to F02
renumbering C07 to F03
renumbering C09 to F04
renumbering D02 to F05
renumbering D03 to F06
renumbering E04 to F07
renumbering E09 to F08
renumbering E10 to F09
renumbering E11 to F10
renumbering F01 to F11
renumbering F02 to F12
renumbering F05 to F13
renumbering F06 to F14
renumbering F09 to F15
renumbering F10 to F16
renumbering F11 to G01
renumbering G09 to G02
renumbering G13 to G03
renumbering G14 to G04
renumbering H01 to G05
renumbering H02 to G06
renumbering H03 to G07
renumbering H09 to G08
renumbering H12 to G09
|#

#|

uwlab2:

renumbering E05 to H01
renumbering F05 to H02
renumbering F06 to H03
renumbering F07 to H04
|#
    
(defun read-files ()
  (make-object 'group :name "hase patterns"
	       :objects (import-file "/Users/mnl/siff-svn/md/hase-patterns.syx"
				     "/users/mnl/siff-svn/md/factory-wavs/wavs/*.wav"))
  (make-object 'group :name "short smapz"
	       :objects (import-file "/Users/mnl/siff-svn/md/08-09-2007/short-smapz/backup.syx"
				     "/Users/mnl/siff-svn/md/08-09-2007/short-smapz/*.wav"))
  (make-object 'group :name "alles moegliche"
	       :objects (import-file "/Users/mnl/siff-svn/md/29-07-2007/backup.syx"
				     "/Users/mnl/siff-svn/md/29-07-2007/*.wav"))
  (make-object 'group :name "alles moegliche quixit"
	       :objects (import-file "/Users/mnl/siff-svn/md/29-07-2007-quixit-50/backup.syx"
				     "/Users/mnl/siff-svn/md/29-07-2007-quixit-50/*.wav"))
  (make-object 'group :name "md solo"
	       :objects (import-file "/Users/mnl/siff-svn/md/solo-md/backup.syx"
				     "/Users/mnl/siff-svn/md/solo-md/*.wav"))
  (make-object 'group :name "uwlab"
	       :objects (import-file "/Users/mnl/siff-svn/md/23-09-2007-uw-lab/backup.syx"
				     "/Users/mnl/siff-svn/md/23-09-2007-uw-lab/uw-lab/real-wavs/*.wav")))

(defparameter *final-sample-list*
  '("rom-05 alles export.wav"  ; kick
    "rom-05 hase export.wav"   ; snare
    "rom-22 alles export.wav"  ; hat
    "rom-23 alles export.wav"  ; open hat
    "rom-01 alles export.wav"  ; heavy clap
    "rom-02 alles export.wav"  ; 50 cents haa
    "rom-06 alles export.wav"  ; garage bass
    "rom-07 alles export.wav"  ; dungdung
    "rom-08 alles export.wav"  ; snap
    "rom-16 short smapz export.wav" ; guitar funk
    "rom-19 short smapz export.wav" ; wiiing
    "rom-20 short smapz export.wav" ; cymbal
    "rom-23 short smapz export.wav" ; clap
    "rom-25 short smapz export.wav" ; pad swell
    "rom-26 short smapz export.wav" ; bass hall
    "rom-01 mdsolo export.wav" ; melody
    "rom-02 mdsolo export.wav" ; chord
    "rom-03 mdsolo export.wav" ; bass
    "rom-05 mdsolo export.wav" ; pad swell 2
    "rom-06 mdsolo export.wav" ; bass short
    "rom-08 mdsolo export.wav" ; bass garage
    "rom-10 mdsolo export.wav" ; chord2
    "rom-14 mdsolo export.wav" ; bass higher
    "rom-09 mdsolo export.wav" ; the sky
    "rom-12 mdsolo export.wav" ; shake your ass
    "rom-03 uwlab export.wav" ; bass short 2
    "rom-07 uwlab export.wav" ; rhodes
    "rom-13 uwlab export.wav" ; guitar funk
    "rom-14 uwlab export.wav"  ; long pad
    "rom-06 uwlab export.wav" ; i am calling you
    "rom-10 uwlab export.wav" ; all blow out
    "rom-32 short smapz export.wav" ; one two three four
))

(defun read-exported (subdirs &key exportdir)
  (loop for subdir in subdirs
       for dir = (merge-pathnames (make-pathname :directory (list :relative subdir))
				  exportdir)
     appending (import-file (make-pathname :name "output" :type "syx"
							  :defaults dir)
			    (directory (make-pathname :name :wild :type "wav"
								  :defaults dir)))))

(defun read-edinburgh-exported ()
  (read-exported '("hase-export" "alles-export" "short-export" "mdsolo-export" "uwlab-export")
		 :exportdir "/users/mnl/siff-svn/md/edinburgh/exports/"))



(defun export-song (song outputdir)
  (export-group (append (list song)
			(song-patterns song)
			(song-kits song)) outputdir))
	       

(defparameter *hase* (group-with-name "hase patterns"))
(defparameter *short-smapz* (group-with-name "short smapz"))
(defparameter *alles* (group-with-name "alles moegliche"))
(defparameter *alles-quixit* (group-with-name "alles moegliche quixit"))
(defparameter *uwlab* (group-with-name "uwlab"))
(defparameter *mdsolo* (group-with-name "md solo"))

(defparameter *hase-pats* (get-patterns 
			   '(:A05 :A02 :A10 :A13 :A01
			     :A03 :B05 :B04 :B03 :B02
			     :B01 :A07 :A03 :A01 :A08
			     :A09 :A07) (group-patterns *hase*)))

(defparameter *hase-export-group*
  (group-with-name "hase export"))

(defparameter *short-pats* (let ((pats (group-patterns *short-smapz*)))
			     (get-patterns '(:C05 :C06 :C07 :C09
					     :E09 :E10 :E11
					     :D02 :D03
					     :F01 :F02
					     :F05 :F06
					     :F09 :F10 :F11
					     :G09
					     :G13 :G14
					     :H09 :H12
					     :E04
					     :H01 :H02 :H03)
					   pats)))
(defparameter *short-songs* (list (third (group-songs *short-smapz*))))

(defparameter *short-export-group*
  (group-with-name "short smapz export"))


(defparameter *alles-pats* (get-patterns
			    '(:G01 :G02 :G03
			      :A01 :A02 :A03 :A04 :A05 :A09 :A11
			      :D02 :D03 :D04 :D06 :D08 :D10)
			    (group-patterns *alles*)))

(defparameter *alles-export-group*
  (group-with-name "alles export"))


(defparameter *md-solo-export-group*
  (group-with-name "md solo export"))


(defparameter *uwlab-songs* (songs-with-name "UWLAB03"))

(defparameter *uwlab-pats* (get-patterns
			    '(:E05) (group-patterns *uwlab*)))

(defparameter *uwlab-export-group*
  (group-with-name "uwlab export"))


(defun make-groups ()
  (make-object 'group :name "uwlab export"
	       :objects (append
			 *uwlab-pats*
			 (group-songs *uwlab*)))
  (make-object 'group :name "md solo export"
	       :objects (group-objects *mdsolo*))
  (make-object 'group :name "alles export"
	       :objects *alles-pats*)
  (make-object 'group :name "short smapz export"
	       :objects (append *short-pats*
				*short-songs*))
  (make-object 'group :name "hase export"
	       :objects *hase-pats*))
  
  

(defun export-all-groups (&key (dir "/tmp/exports/"))
  (export-group *hase-export-group* (merge-pathnames
				     (make-pathname :directory '(:relative "hase-export"))
				     dir) :force t)

  (export-group *short-export-group* (merge-pathnames
				     (make-pathname :directory '(:relative "short-export"))
				     dir) :force t)

  (export-group *alles-export-group* (merge-pathnames
				     (make-pathname :directory '(:relative "alles-export"))
				     dir) :force t)

  (export-group *uwlab-export-group* (merge-pathnames
				     (make-pathname :directory '(:relative "uwlab-export"))
				     dir) :force t)

  (export-group *md-solo-export-group* (merge-pathnames
				     (make-pathname :directory '(:relative "mdsolo-export"))
				     dir) :force t))


(defparameter *exported* (group-with-name "exported"))

(defparameter *rename-list*
  '(("rom-02 hase export.wav" "rom-05 alles export.wav")
    ("rom-04 hase export.wav" "rom-05 alles export.wav")
    ("rom-11 hase export.wav" "rom-22 alles export.wav")

    ("rom-03 alles export.wav" "rom-22 alles export.wav")
    ("rom-04 alles export.wav" "rom-22 alles export.wav")
    ("rom-31 alles export.wav" "rom-22 alles export.wav")
    ("rom-09 alles export.wav" "rom-05 hase export.wav")
    ("rom-29 alles export.wav" "rom-05 hase export.wav")
    ("rom-15 alles export.wav" "rom-05 hase export.wav")
    ("rom-14 alles export.wav" "rom-05 alles export.wav")
    ("rom-17 alles export.wav" "rom-23 short smapz export.wav")
    ("rom-24 alles export.wav" "rom-23 short smapz export.wav")
    ("rom-25 alles export.wav" "rom-22 alles export.wav")
    ("rom-30 alles export.wav" "rom-08 alles export.wav")
    ("rom-26 alles export.wav" "rom-08 alles export.wav")
    ("rom-10 alles export.wav" "rom-08 alles export.wav")

    ("rom-01 short smapz export.wav" "rom-01 alles export.wav")
    ("rom-02 short smapz export.wav" "rom-02 alles export.wav")
    ("rom-05 short smapz export.wav" "rom-05 alles export.wav")
    ("rom-06 short smapz export.wav" "rom-06 alles export.wav")
    ("rom-07 short smapz export.wav" "rom-07 alles export.wav")
    ("rom-08 short smapz export.wav" "rom-08 alles export.wav")
    ("rom-03 short smapz export.wav" "rom-22 alles export.wav")
    ("rom-04 short smapz export.wav" "rom-22 alles export.wav")
    ("rom-24 short smapz export.wav" "rom-22 alles export.wav")
    ("rom-22 short smapz export.wav" "rom-05 hase export.wav")
    ("rom-09 short smapz export.wav" "rom-05 hase export.wav")
    ("rom-17 short smapz export.wav" "rom-05 hase export.wav")
    ("rom-14 short smapz export.wav" "rom-05 alles export.wav")
    ("rom-18 short smapz export.wav" "rom-08 alles export.wav")

    ("rom-04 mdsolo export.wav" "rom-10 mdsolo export.wav")
    ("rom-07 mdsolo export.wav" "rom-14 mdsolo export.wav")
    ("rom-13 mdsolo export.wav" "rom-06 mdsolo export.wav")
    ("rom-11 mdsolo export.wav" "rom-05 alles export.wav")

    ("rom-04 uwlab export.wav" "rom-08 alles export.wav")
    ("rom-11 uwlab export.wav" "rom-05 alles export.wav")
    ("rom-15 uwlab export.wav" "rom-05 hase export.wav")
    ("rom-12 uwlab export.wav" "rom-22 alles export.wav")))
    
(defun rename-samples ()
  (loop for (old-sample new-sample) in *rename-list*
       do (replace-sample *exported* old-sample new-sample)
       (replace-sample *exported* new-sample new-sample)))

(defun write-final-samples (dir)
  (loop for sample in *final-sample-list*
       for i from 1
       for name = (format nil "rom-~2,'0d.wav" i)
       do (copy-file (merge-pathnames sample "/users/mnl/siff-svn/md/edinburgh/exports/all-exports/")
		     (merge-pathnames name dir))))

;; uwlab E05 F05 F06 F07
;; song UWLAB03
		      
;; all except newscool 2
;; 8-> 4, 9 -> 0, 10 -> 1, 11 -> 3, 12 -> 5, 13 -> 6, 14 -> 7

;; adjust volume on all machines
;; ROM-05 factory -> hihat
;; ROM-04 faactory -> hit + cymbal
;; ROM-02 factory -> snare
;; ROM-11 factory -> hihat

;; short smapz group
;; C05 C06 C07 C09
;;  E09 E10 E11 -> check song
;; maybe D02 D03
;; mit reaktor F01 F02
;; minimal F05 F06
;; maybe F09, F10, F11
;; PI G09
;; minimal G13, G14
;; H09 loungey beat
;; H12 minimal aber erweitern auf 32
;; G05 jam
;; H01 H02 H03 mit song "new song (2)" jetzt "h01h02song"
;; E04 madness

;; 29-07
;; F05 - minimal
;; G01, G02, G03 - minimal
;; A01-A05 mit synth
;; A09, A11 mit synth
;; D02 - D03 - D04
;; D06 dubstep mit synth
;; D08 mit synth disko
;; D10 vielleicht minimal?

;; 29-07 quixit
;; D02 - D04