(in-package :md)

;;; XXX kit identical

;;; store data

(defvar *current-user* nil)

(eval-when (:Compile-toplevel :load-toplevel :execute)
  (defparameter *montezuma-index*
    (make-instance 'montezuma:index
		   :default-search-field "*")))

;;; montezuma index class XXX

(define-persistent-class md-user ()
  ((name :read :index-type string-unique-index
	 :index-reader md-user-with-name
	 :index-values all-md-users)
   (email :update :index-type string-unique-index
	  :index-reader md-user-with-email)
   (password :update :initform "")
   (description :update :initform "")
   (avatar :update :initform nil)))

(deftransaction md-user-set-description (user description)
  (setf (md-user-description user) description))

(deftransaction md-user-set-avatar (user avatar)
  (setf (md-user-avatar user) avatar))

(define-persistent-class md-mail ()
  ((from :read :index-type hash-index
	 :index-initargs (:test #'equal)
	 :index-reader md-mails-from)
   (to :read :index-type hash-index
	 :index-initargs (:test #'equal)
       :index-reader md-mails-to)
   (creation-time :update :initform (get-universal-time))
   (title :update :initform "")
   (body :update :initform "")))
	 

(define-persistent-class md-comment ()
  ((user :read :index-type hash-index
	 :index-initargs (:test #'equal)
	 :index-reader md-comments-of-user)
   (title :update :initform ""
	  :montezuma-index :tokenized)
   (body :update :initform ""
	 :montezuma-index :tokenized
	 :montezuma-stored t)
   (creation-time :update :initform (get-universal-time))
   (edit-time :update :initform nil)
   (parent :read :index-type hash-index
		 :index-initargs (:test #'equal)
	   :index-reader comments-for-md-object))
  (:class-indices (montezuma :index-type montezuma-index
			     :slots (title body)
			     :index-subclasses t
			     :index-initargs (:index *montezuma-index*)
			     :index-reader montezuma-get))
  (:metaclass montezuma-persistent-class))

(defun montezuma-get (index query &rest options)
  (let ((res))
    (montezuma:search-each index query
			   #'(lambda (doc score)
			       (push (list (montezuma-doc-object (montezuma-index-index index) doc) score) res)))
    res))

(deftransaction md-comment-set-title (comment new-title)
  (with-slots (title edit-time) comment
      (setf title new-title
	    edit-time (get-universal-time))))

(deftransaction md-comment-set-body (comment new-body)
  (with-slots (body edit-time) comment
    (setf body new-body
	  edit-time (get-universal-time))))

(defvar *current-import* nil)

(defun symbol-to-lowercase (symbol)
  (string-downcase (symbol-name symbol)))

(define-persistent-class md-object ()
  ((user :read :initform *current-user* :index-type hash-index
					:index-initargs (:test #'equal)
	 :index-reader md-objects-of-user)
   
   (name :update
	 :index-type hash-index
	 :index-initargs (:test #'equal)
	 :index-reader md-objects-with-name
	 :montezuma-index :tokenized)
   (type :read
         :index-type hash-index
	 :index-reader md-objects-with-type
	 :montezuma-index :untokenized
	 :montezuma-convert symbol-to-lowercase)
   (downloads :update :initform 0)
   (keywords :update :initform ""
	     :montezuma-index :tokenized)
   (import :read :initform *current-import*)
   (creation-time :update :initform (get-universal-time))
   (edit-time :update :initform 0)
   (description :update :initform ""
		:montezuma-index :tokenized)
   (comments :update :initform nil)
   (public :update :initform nil)
   (ratings :update :initform nil))
  (:class-indices (montezuma :index-type montezuma-index
			     :index-subclasses t
			     :index-initargs (:index *montezuma-index*)
			     :index-reader montezuma-get))
  (:metaclass montezuma-persistent-class))
   
(deftransaction md-object-set-name (object new-name)
  (with-slots (name edit-time) object
    (setf name new-name
	  edit-time (get-universal-time))))

(defmethod md-object-rating ((object md-object))
  (loop for rating in (md-object-ratings object)
       for count from 1
     summing rating into sum
       finally (return (/ sum count))))

(deftransaction md-object-add-rating (object rating)
  (push rating (md-object-ratings object)))

(deftransaction md-object-set-description (object new-description)
  (with-slots (description edit-time) object
    (setf description new-description
	  edit-time (get-universal-time))))

(deftransaction md-object-add-new-comment (object user title body)
  (let ((comment (make-instance 'md-comment :user user :title title :body body)))
    (md-object-add-comment object comment)))

(deftransaction md-object-add-comment (object comment)
  (push comment (md-object-comments object)))

(deftransaction md-object-remove-comment (object comment)
  (with-slots (comments) object
    (setf comments (remove comment comments :test #'equal))))


(define-persistent-class md-import (md-object blob)
  ((name :update :index-type hash-index
		 :index-initargs (:test #'equal)
		 :index-reader md-imports-with-name)
   (type :read :initform :import)
   (objects :update :initform nil))
  (:metaclass montezuma-persistent-class))

(defmethod md-object-empty-p ((obj md-object))
  nil)

(defun all-md-imports ()
  (md-objects-with-type :import))

(defun md-import-file (user file description)
  (let* ((*current-user* user)
	 (name (pathname-name file))
	 (import (make-blob-from-file file 'md-import
				      :name name
				      :user user
				      :description description))
	 (objects (read-elektron-file file)))
    (setf objects (loop for obj in objects
		     if (md-object-empty-p obj)
		       do (delete-object obj)
		       else
		       collect obj))
    (change-slot-values import 'objects objects)
    import))

;;; md model

(defun track-name (track)
  (if (keywordp track)
      track
      (elt *track-names* track)))

(defun lfo-type-name (type)
  (if (keywordp type)
      type
      (elt '(:FREE :TRIG :HOLD) type)))

(defun eql-or-member (key1 key2)
  (cond ((listp key1)
	 (member key2 key1))
	((listp key2)
	 (member key1 key2))
	(t (eql key1 key2))))

(defun rom-model-p (model)
  (member (machine-name model)
	  '(:ROM-01 :ROM-02 :ROM-03 :ROM-04
	    :ROM-05 :ROM-06 :ROM-07 :ROM-08
	    :ROM-09 :ROM-10 :ROM-11 :ROM-12
	    :ROM-13 :ROM-14 :ROM-15 :ROM-16      
	    :ROM-17 :ROM-18 :ROM-19 :ROM-20
	    :ROM-21 :ROM-22 :ROM-23 :ROM-24
	    :ROM-25 :ROM-26 :ROM-27 :ROM-28
	    :ROM-29 :ROM-30 :ROM-31 :ROM-32)))

(defun rom-num-model (num)
  (machine-num (elt '(:ROM-01 :ROM-02 :ROM-03 :ROM-04
	    :ROM-05 :ROM-06 :ROM-07 :ROM-08
	    :ROM-09 :ROM-10 :ROM-11 :ROM-12
	    :ROM-13 :ROM-14 :ROM-15 :ROM-16      
	    :ROM-17 :ROM-18 :ROM-19 :ROM-20
	    :ROM-21 :ROM-22 :ROM-23 :ROM-24
	    :ROM-25 :ROM-26 :ROM-27 :ROM-28
	    :ROM-29 :ROM-30 :ROM-31 :ROM-32) num)))

(defun rom-model-num (model)
  (index-of 	  '(:ROM-01 :ROM-02 :ROM-03 :ROM-04
	    :ROM-05 :ROM-06 :ROM-07 :ROM-08
	    :ROM-09 :ROM-10 :ROM-11 :ROM-12
	    :ROM-13 :ROM-14 :ROM-15 :ROM-16      
	    :ROM-17 :ROM-18 :ROM-19 :ROM-20
	    :ROM-21 :ROM-22 :ROM-23 :ROM-24
	    :ROM-25 :ROM-26 :ROM-27 :ROM-28
	    :ROM-29 :ROM-30 :ROM-31 :ROM-32)
		  (machine-name model)))

(defun model-param-name (model param)
  (let ((model-assoc (cadr (assoc (machine-name model) *model-param-names* :test #'eql-or-member))))
    (when model-assoc
      (cadr (assoc param model-assoc)))))
    
(defun param-name (model param)
  (if (keywordp param)
      param
      (let ((param2 (model-param-name model param)))
	(if param2
	    param2
	    (or (cadr (assoc param *param-to-name*)) param)))))

(defun pattern-name (pattern)
  (cond ((keywordp pattern) pattern)
	((typep pattern 'md-pattern)
	 (pattern-name (md-pattern-position pattern)))
	(t
	 (cadr (assoc pattern *pattern-pos-to-name*)))))

(defun pattern-num (pattern)
  (cond ((numberp pattern) pattern)
	((keywordp pattern) (car (inv-assoc pattern *pattern-pos-to-name*)))
	((typep pattern 'md-pattern)
	 (md-pattern-position pattern))))

(defun find-pattern (name patterns)
  (find (pattern-num name) (remove #'(lambda (x)
				       (typep x 'md-pattern))
				   patterns)
	:key #'md-pattern-position))

(defun find-pattern-kit (name patterns)
  (let ((pattern (find-pattern name patterns)))
    (when pattern
      (md-pattern-kit pattern))))

(defun find-pattern-track (name track patterns)
  (let ((pattern (find-pattern name patterns)))
    (when pattern
      (elt (md-pattern-tracks pattern) (1- track)))))

(defun md-pattern-track-machine (track)
  (let ((kit (md-pattern-kit (md-pattern-track-pattern track)))
	(pos (md-pattern-track-index track)))
    (elt (kit-machines kit) pos)))

(defun find-pattern-track-machine (name track patterns)
  (let ((pattern (find-pattern name patterns)))
    (when pattern
      (let ((kit (md-pattern-kit pattern)))
	(elt (kit-machines kit) (1- track))))))

(defun machine-name (machine)
  (cond ((keywordp machine) machine)
	((typep machine 'machine)
	 (machine-name (machine-model machine)))
	(t (cadr (assoc machine *machine-to-name*)))))

(defun machine-num (machine)
  (if (numberp machine)
      machine
      (car (inv-assoc machine *machine-to-name*))))

(defparameter *kit-hash* (make-hash-table))

(defgeneric md-object-empty-p (obj))

(define-persistent-class kit (md-object)
  ((type :read :initform :kit)
   (position :update :initform 0)
   (name :update :index-type hash-index
		 :index-initargs (:test #'equal)
	 :index-reader kits-with-name)
   (machines :update :initform nil)
   (lfos :update :initform nil)
   (reverb :update :initform nil)
   (delay :update :initform nil)
   (eq :update :initform nil)
   (dynamics :update :initform nil))
  (:metaclass montezuma-persistent-class))

(defun fill-kit-with-empty-machines (kit)
  (change-slot-values kit 'machines
		      (loop for i from 0
			 for machine in (fill-list (kit-machines kit) 16)
			 collect (if machine machine
				     (make-empty-machine i :kit kit))))
  (change-slot-values kit 'lfos
		      (mapcar #'machine-lfo (kit-machines kit)))
  kit)


(defun list-equal (l1 l2 &key (test #'eql))
  (do ((elt1 l1 (cdr elt1))
       (elt2 l2 (cdr elt2)))
      ((or (null elt1) (null elt2))
       (when (and (null elt1) (null elt2)) t))
    (unless (funcall test (car elt1) (car elt2))
      (return-from list-equal nil))))

(defmethod kit= ((k1 kit) (k2 kit))
  (and (string= (kit-name k1) (kit-name k2))
       (equal (kit-reverb k1)   (kit-reverb k2))
       (equal (kit-delay k1)    (kit-delay k2))
       (equal (kit-eq k1)       (kit-eq k2))
       (equal (kit-dynamics k1) (kit-dynamics k2))
       (list-equal (kit-machines k1) (kit-machines k2)
		   :test #'machine=)))
       
(defmethod destroy-object :before ((kit kit))
  (dolist (machine (kit-machines kit))
    (destroy-object machine))
  (dolist (lfo (kit-lfos kit))
    (destroy-object lfo)))

(defmethod md-object-empty-p ((kit kit))
  (and (slot-boundp kit 'machines)
       (slot-boundp kit 'lfos)
       (every #'md-object-empty-p (kit-machines kit))
       (every #'identity
	      (loop for lfo in (kit-lfos kit)
		 for i from 0
		   collect (= (lfo-destination lfo) i)))))
	    

(defmethod initialize-instance :after ((kit kit) &rest args)
  (declare (ignore args))
  (when *kit-hash*
    (setf (gethash (kit-position kit) *kit-hash*) kit)))

(defmethod kit-param-value ((kit kit) track param)
  (elt (machine-params (elt (kit-machines kit) track)) param))

(defmethod print-object ((kit kit) stream)
  (print-unreadable-object (kit stream :type t)
    (format stream "NAME: ~D ID: ~D POS: ~D" (kit-name kit) (store-object-id kit)
	    (kit-position kit))))

(defun model-name-string (machine)
  (symbol-name (machine-name machine)))

(define-persistent-class machine (md-object)
  ((type :read :initform :machine :reader md-object-type)
   (params :update)
   (index :update :initform 0)
   (kit :update :initform nil)
   (lfo :update :initform nil)
   (sample :read :initform nil)
   (model :update :index-type hash-index
	  :index-reader machines-with-model
	  :montezuma-index :tokenized
	  :montezuma-convert model-name-string)
   (level :update :initform 0)
   (trig :update :initform -1)
   (mute :update :initform -1))
  (:metaclass montezuma-persistent-class))

(defun make-empty-machine (index &key kit)
  (make-instance 'machine
	       :params '(64 64 0 0 0 0 0 0 0 0 64 64 0 127 0 0 0 127 64 0 0 64 0 0)
	       :index index
	       :kit kit
	       :lfo (make-empty-lfo :kit kit :index index)
	       :trig 255
	       :mute 255
	       :level 100
	       :model 0))

(defmethod machine= ((m1 machine) (m2 machine))
  (and (equal (machine-params m1) (machine-params m2))
       (= (machine-index m1) (machine-index m2))
       (if (and (machine-sample m1)
		(machine-sample m2))
	   (string= (machine-sample m1) (machine-sample m2))
	   t)
       (= (machine-model m1) (machine-model m2))
       (= (machine-level m1) (machine-level m2))
       (= (machine-trig m1) (machine-trig m2))
       (= (machine-mute m1) (machine-mute m2))
       (lfo= (machine-lfo m1) (machine-lfo m2))))


(defmethod md-object-empty-p ((machine machine))
  (= (machine-model machine) 0))

(defmethod print-object ((track machine) stream)
  (print-unreadable-object (track stream :type t)
    (format stream "~A (LEV: ~A)" (machine-name (machine-model track)) (machine-level track))))

(defmethod machine-param-value ((track machine) param)
  (elt (machine-params track) param))

(define-persistent-class lfo ()
  ((kit :update :initform nil)
   (index :update :initform 0)
   (destination :update :initform 0)
   (param :update :initform 0)
   (shape1 :update :initform 0)
   (shape2 :update :initform 0)
   (type :update :initform :free)
   (state :update :initform nil)))

(defun make-empty-lfo (&key kit index)
  (make-instance 'lfo
	       :kit kit
	       :index index
	       :destination 0
	       :param 0
	       :shape1 0
	       :shape2 4
	       :type 0
	       :state '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			0 0 0 0 0 0 0 0 2 154 0 0 0 0)))
  

(defmethod lfo= ((l1 lfo) (l2 lfo))
  (and (= (lfo-index l1) (lfo-index l2))
       (= (lfo-destination l1) (lfo-destination l2))
       (= (lfo-param l1) (lfo-param l2))
       (= (lfo-shape1 l1) (lfo-shape1 l2))
       (= (lfo-shape2 l1) (lfo-shape2 l2))
       (= (lfo-type l1) (lfo-type l2))
       (equal (lfo-state l1) (lfo-state l2))))

(defun make-machine (params index model level trig mute)
  (let ((sample nil))
    (when (rom-model-p model)
      (let ((num (rom-model-num model)))
	(if (> (length *samples*) num)
	    (setf sample (namestring (elt *samples* num)))
	    (warn "could not find smaple for slot ~A~%" num))
      ))
    (make-instance 'machine
		 :index index
		 :sample sample
		 :params params :model model :level level
		 :trig trig :mute mute)))

(defun make-lfo (index lfo-data)
  (make-instance 'lfo
	       :index index
	       :destination (elt lfo-data 0)
	       :param (elt lfo-data 1)
	       :shape1 (elt lfo-data 2)
	       :shape2 (elt lfo-data 3)
	       :type (elt lfo-data 4)
	       :state (nthcdr 5 lfo-data)))

(defun collect-plocks-info (tracks)
  (loop for i from 0
       for track in tracks
       nconc (mapcar #'(lambda (x) (list i x)) (track-plocks-from-pattern track))))

(defun make-md-pattern-track (pattern index data plock-pattern accents slides swings)
  (make-instance 'md-pattern-track
	       :index index
	       :pattern pattern
	       :hits data
	       :plock-pattern plock-pattern
	       :accent accents
	       :swing swings
	       :slide slides))

(define-persistent-class plock ()
  ((track :update :initform nil)
   (param :update :initform 0)
   (vals :update :initform nil)))

(defmethod print-object ((plock plock) stream)
  (with-slots (param vals) plock
    (print-unreadable-object (plock stream :type t)
      (format stream "PARAM ~A: ~A" param vals))))

(defmethod plock-pattern ((plock plock))
  (pad-to (plock-vals plock) 32 :val 0))

(define-persistent-class md-pattern-track ()
  ((pattern :update :initform nil)
   (index :update :initform 0)
   (hits :update :initform nil)
   (accent :update :initform nil)
   (swing :update :initform nil)
   (slide :update :initform nil)
   (plock-pattern :update :initform nil)
   (plocks :update :initform nil)))

(defun make-empty-pattern-track (index &key pattern)
  (make-instance 'md-pattern-track
	       :index index
	       :pattern pattern
	       :hits '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	       :accent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	       :swing '(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
			1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
	       :slide '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	       :plock-pattern '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
				0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	       :plocks nil))

(defmethod destroy-object :before ((track md-pattern-track))
  (dolist (plock (md-pattern-track-plocks track))
    (destroy-object plock)))

(defmethod md-object-empty-p ((track md-pattern-track))
  (every #'(lambda (x) (= x 0)) (md-pattern-track-hits track)))

(defmethod track-length ((track md-pattern-track))
  (length (md-pattern-track-hits track)))

(defun track-plocks-from-pattern (track)
  (let ((res (list)))
    (dotimes (i 24)
      (when (= (elt (md-pattern-track-plock-pattern track) i) 1)
	(push i res)))
    (nreverse res)))

(defun track-plocks-on-step (track step)
  (with-slots (plocks hits plock-pattern) track
    (when (= (elt hits step) 1)
      (remove-if #'(lambda (x) (= (elt (plock-vals x) step) 255)) plocks))))

(define-persistent-class md-pattern (md-object)
  ((type :read :initform :pattern)
   (tracks :update :initform nil)
   (accent-track :update :initform nil)
   (accent :update :initform 0)
   (swing-track :update :initform nil)
   (slide-track :update :initform nil)
   (swing :update :initform nil)
   (kit :update :initform 0
	:index-type hash-index
	:index-reader patterns-with-kit)
   (scale :update :initform 0)
   (double-tempo :update :initform nil)
   (length :update :initform 0)
   (position :update :initform 0))
  (:metaclass montezuma-persistent-class))

(defun fill-pattern-with-empty-tracks (pattern)
  (change-slot-values pattern
		      'tracks (loop for i from 0
				   for track in (fill-list (md-pattern-tracks pattern) 16)
				   collect (if track track
					       (make-empty-pattern-track
						i :pattern pattern))))
  pattern)

(defmethod initialize-instance :after ((pattern md-pattern) &rest args)
  (declare (ignore args))
  (when *pattern-hash*
    (format t "setting ~A in hash to ~A~%" (md-pattern-position pattern) pattern)
    (setf (gethash (md-pattern-position pattern) *pattern-hash*) pattern)))

(defmethod destroy-object :before ((pattern md-pattern))
  (dolist (track (md-pattern-tracks pattern))
    (destroy-object track)))

(defmethod md-object-empty-p ((pattern md-pattern))
  (every #'md-object-empty-p (md-pattern-tracks pattern)))

(defmethod clear-track ((track md-pattern-track))
  (with-slots (hits plocks) track
    (setf hits (split-beat '(0 0 0 0))
	  plocks nil)))

(defmethod clear-pattern ((pat md-pattern))
  (dolist (track (md-pattern-tracks pat))
    (clear-track track)))

(defun copy-pattern (pat)
  (let ((res (copy-object pat)))
    (clear-pattern res)
    res))

(defmethod print-object ((pat md-pattern) stream)
  (print-unreadable-object (pat stream :type t :identity t)
    (format stream "POS: ~A" (pattern-name (md-pattern-position pat)))))
	       

#+nil(defmethod md-pattern-length ((pattern md-pattern))
  (track-length (first (md-pattern-tracks pattern))))

(defmethod md-pattern-scale ((pattern md-pattern))
  (if (> (md-pattern-length pattern) 16)
      1 0))

(defmethod md-pattern-plocks ((pat md-pattern))
  (loop for track-idx from 0
     for track in (md-pattern-tracks pat)
     nconc (mapcar #'(lambda (plock)
		       (list track-idx (plock-param plock))) (md-pattern-track-plocks track))))

(defun row-pattern-to-byte (pattern)
  (case pattern
    (:END  #xff)
    (:JUMP #xfe)
    (:REMARK #xfd)
    (t (pattern-num pattern))))

(defun byte-to-row-pattern (pattern)
  (case pattern
    (#xFF :END)
    (#xFE :JUMP)
    (#xFD :REMARK)
    (t (if *pattern-hash*
	   (or (gethash pattern *pattern-hash*) pattern)
	   pattern))))

(define-persistent-class md-row ()
  ((song :update :initform nil)
   (index :update :initform 0)
   (pattern :update :initform :END)
   (kit :update :initform 0)
   (loop-times :update :initform 0)
   (jump-destination :update :initform 0)
   (mutes :update :initform nil)
   (tempo :update :initform 65535)
   (start :update :initform 0)
   (stop :update :initform 16)))


(defun make-end-row (song index kit)
  (make-instance 'md-row :song song :index index
	       :pattern :end
	       :kit kit
	       ))

(defmethod print-object ((row md-row) stream)
  (print-unreadable-object (row stream :type t)
    (with-slots (pattern loop-times jump-destination start stop) row
      (case pattern
	(:END (format stream "END"))
	(:JUMP (if (> loop-times 0)
		   (format stream "LOOP ~A X -> ~A" loop-times jump-destination)
		   (format stream "JMP -> ~A" jump-destination)))
	(:REMARK (format stream "REMARK"))
	(t (format stream "PAT ~A [~A -> ~A]" (pattern-name pattern) start stop))))))

(define-persistent-class md-song (md-object)
  ((type :read :initform :song)
   (name :update :initform ""
	 :index-type hash-index
	 :index-initargs (:test #'equal)
	 :index-reader songs-with-name)
   (position :update :initform 0)
   (rows :update :initform nil))
  (:metaclass montezuma-persistent-class))
  

(defmethod destroy-object :before ((song md-song))
  (dolist (row (md-song-rows song))
    (destroy-object row)))

(defmethod md-object-empty-p ((song md-song))
  (and (= (length (md-song-rows song)) 1)
       (eql (md-row-pattern (first (md-song-rows song))) :END)))

(defmethod print-object ((song md-song) stream)
  (print-unreadable-object (song stream :type t)
    (format stream "~S (~A)" (md-song-name song) (md-song-position song))))

(define-persistent-class group (md-object)
  ((objects :update :initform nil))
  (:metaclass montezuma-persistent-class))

(defmethod print-object ((group group) stream)
  (print-unreadable-object (group stream :type t)
    (format stream "~S (~A objects)" (store-object-id group) (length (group-objects group)))))
