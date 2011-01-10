(in-package :md)

;;; midi conversion ; midi -> md

;;; midi file

(defun event-filter (event)
  (or (typep event 'midi)
      (typep event 'midi-control-change)))

(Defun chan-filter (event)
  (or (typep event 'midi)
      (typep event 'cm::midi-channel-event)))

(defun step-filter (event)
  (< (int-near (object-time event) 0.125) 0.01))

(defun read-events (file)
  (let ((events (subobjects (import-events file))))
    (remove-if-not (and-fgen #'step-filter #'event-filter) events)))

(defun read-events-all (file)
  (let ((events (subobjects (import-events file))))
    (remove-if-not (and-fgen #'step-filter #'chan-filter) events)))

(defparameter *pattern-midi-length* 4.0)

(defun split-into-patterns (events)
  (let ((res (list)))
    (loop with start-time = 0
	 with pattern = (list)
       for evt in events
       for evt2 = (copy-object evt)
       do (decf (object-time evt2) start-time)
       when (< (object-time evt2) *pattern-midi-length*)
       do (push evt2 pattern)
       else
       do (progn (push (nreverse pattern) res)
		 (incf start-time *pattern-midi-length*)
		 (decf (object-time evt2) *pattern-midi-length*)
		 (setf pattern (list evt2)))
       finally (push (nreverse pattern) res))
    (nreverse res)))

(defun events-length (events)
  (1+ (ceiling (* 8 (reduce #'max (mapcar #'object-time events) :initial-value 0)))))

(defun step-events (events step)
  (remove-if-not #'(lambda (x) (= (object-time x) (/ step 8))) events))

(defun events-notes (events)
  (remove-if-not #'(lambda (x) (typep x 'midi)) events))

(defun events-ccs (events)
  (remove-if-not #'(lambda (x) (typep x 'midi-control-change)) events))

(defmethod set-event ((pat md-pattern) (note midi) step)
  (let ((tracknum (keynum-to-track (cm::midi-keynum note))))
    (unless (null tracknum)
      (let ((track (elt (md-pattern-tracks pat) tracknum)))
	(setf (elt (md-pattern-track-hits track) step) 1)))))

(defmethod set-event ((pat md-pattern) (cc midi-control-change) step)
  (let* ((track-plock (chan-cc-to-track-plock (cm::midi-event-channel cc)
					      (cm::midi-event-data1 cc))))
    (unless (null track-plock)
      (let* ((tracknum (first track-plock))
	     (param (second track-plock))
	     (track (elt (md-pattern-tracks pat) tracknum))
	     (plock (track-find-plock track param)))
	(setf (elt (plock-vals plock) step) (cm::midi-event-data2 cc))))))

(defun track-find-plock (track param)
  (let ((res (find-if #'(lambda (x) (= (plock-param x) param))
		      (md-pattern-track-plocks track))))
    (if res
	res
	(progn
	  (setf res (make-instance 'plock :param param
				   :vals (make-list 32 :initial-element 255)))
	  (push res (md-pattern-track-plocks track))
	  res))))

(defun voice-from-events (events voice)
  (let ((len (events-length events))
	(res (list)))
    (dotimes (step len)
      (let ((evts (remove-if-not #'(lambda (x) (typep x 'midi))
				 (step-events events step))))
	(when (> (length evts) voice)
	  (push (elt evts voice) res))))
    (nreverse res)))

(defun pattern-from-events (pat events)
  (let ((res (copy-pattern pat)))
    (setf (md-pattern-length res) (events-length events)
	  (md-pattern-scale res) (if (> (md-pattern-length res) 16) 1 0))
    (dotimes (step (md-pattern-length res))
      (mapcar #'(lambda (x) (set-event res x step))
	      (step-events events step)))
    res))

(defgeneric convert-event-synth (evt mapping dest &key ctr-8p-pitch ctr-8p-dec))

(defmethod convert-event-synth ((cc midi-control-change) mapping dest-track &key ctr-8p-pitch ctr-8p-dec)
  (list cc))

(defmethod convert-event-synth ((note midi) mapping dest-track &key (ctr-8p-pitch nil)
				(ctr-8p-dec nil))
  (let ((pitch-param (if ctr-8p-pitch
			 (track-plock-to-cc dest-track ctr-8p-pitch)
			 (track-plock-to-cc dest-track 0)))
	(dec-param  (if ctr-8p-pitch
			(when ctr-8p-dec
			  (track-plock-to-cc dest-track ctr-8p-dec))
			(track-plock-to-cc dest-track 1))))
    (remove nil
	    (list (new midi :time (object-time note) :keynum (track-to-keynum dest-track)
		       :duration 0.125 :channel 0)
		  (new midi-control-change :time (object-time note)
		       :channel (first pitch-param)
		       :controller (second pitch-param)
		       :value (or (cdr (assoc (to-keyname (cm::midi-keynum note)) mapping)) 0))
		  #+nil(when dec-param
			 (new midi-control-change :time (object-time note)
			      :channel (first dec-param)
			      :controller (second dec-param)
			      :value 60))))))

(defmethod convert-event-synth (bla mapping dest-track &key ctr-8p-pitch ctr-8p-dec)
  nil)

(defun convert-events-synth (events  mapping dest-track)
  (mapcan #'(lambda (evt) (convert-event-synth evt mapping dest-track)) events))

(defun convert-events-poly-synth (events track-mappings)
  (sort (loop for (track mapping) in track-mappings
	   for voice from 0
	   appending (convert-events-synth (voice-from-events events voice) mapping track))
	#'< :key #'object-time))

(defun mid-to-synth-track (pat)
  ())

(defun mid-to-mid-track (pat)
  ())

(defun test-convert ()
  (let ((pat (pattern-from-events *pat* (convert-events-synth *evts* *trx-xt-mapping* 0))))
    (write-pattern-to-file pat "/Users/manuel/e13-new.syx")))



;;; md -> midi

(defparameter *track-to-keynum*
  '((0  #x24)
    (1  #x26)
    (2  #x28)
    (3  #x29)
    (4  #x2b)
    (5  #x2d)
    (6  #x2f)
    (7  #x30)
    (8  #x32)
    (9  #x34)
    (10 #x35)
    (11 #x37)
    (12 #x39)
    (13 #x3b)
    (14 #x3c)
    (15 #x3e)))

(defun track-to-keynum (track)
  (cadr (assoc  track *track-to-keynum*)))

(defun inv-assoc (elt seq &key (test #'eql))
  (loop for i in seq
       when (funcall test (cadr i) elt)
       do (return i)))

(defun keynum-to-track (track)
  (car (inv-assoc track *track-to-keynum*)))

(defparameter *track-to-channel*
  '((0 0) (1 0) (2 0) (3 0)
    (4 1) (5 1) (6 1) (7 1)
    (8 2) (9 2) (10 2) (11 2)
    (12 3) (13 3) (14 3) (15 3)))

(defun track-plock-to-cc (track plock)
  (let ((channel (cadr (assoc track *track-to-channel*)))
	(cc (+ plock (* (mod track 4) 24) 16)))
    (list channel cc)))

(defparameter *cc-chan-to-track-plock*
  (let ((res (list)))
    (dotimes (track 16)
      (dotimes (plock 24)
	(push (list (track-plock-to-cc track plock)
		    (list track plock)) res)))
    res))

(defun chan-cc-to-track-plock (chan cc)
  (cadr (assoc (list chan cc) *cc-chan-to-track-plock* :test #'equal)))

(defgeneric track-events (track pat tracknum step))

(defmethod track-events ((track md-pattern-track) pat tracknum step)
  (let ((res (list)))
    (when (= (elt (md-pattern-track-hits track) step) 1)
      (push (new midi :time (now) :keynum (track-to-keynum tracknum) :duration 0.250
		 :channel 0) res)
      (dolist (plock (md-pattern-track-plocks track))
	(let* ((val (elt (plock-vals plock) step))
	       (chan-cc (track-plock-to-cc tracknum (plock-param plock)))
	       (default-val (if (typep (md-pattern-kit pat) 'kit)
				(kit-param-value (md-pattern-kit pat) tracknum (plock-param plock))
				0)))
	      (push (new midi-control-change :time (now) :channel (first chan-cc)
			 :controller (second chan-cc)
			 :value  (if (= val #xFF) default-val val)) res))))

    ;; default value XXX for ccs and stuff
    res))

(defun pat-events (pat step)
  (loop for i below 16
       for track in (md-pattern-tracks pat)
       nconc (track-events track pat i step)))

(defun pat-process (pat)
  (append
   (list (new midi-time-signature :time 0 :numerator 4 :denominator 4 :clocks 36 :32nds 8)
	 (new midi-text-event :time 0 :type 3 :text ""))
   (list
    (cm::process cm::repeat (md-pattern-length pat)
		 cm::for step cm::from 0
		 cm::for evts = (pat-events pat step)
		 cm::when evts
		 cm::output (pat-events pat step)
		 cm::wait 0.250))
   (when (typep (md-pattern-kit pat) 'kit)
     (let ((plocks (md-pattern-plocks pat))
	   (kit (md-pattern-kit pat)))
       (mapcar #'(lambda (x)
		   (let* ((track (first x))
			  (param (second x))
			  (value (kit-param-value kit track param))
			  (chan-cc (track-plock-to-cc track param)))
		     (new midi-control-change :time 0
			  :channel (first chan-cc)
			  :controller (second chan-cc)
			  :value value)))
	       plocks)))))

