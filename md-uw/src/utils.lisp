(in-package :md)

(defun and-fgen (&rest fns)
  #'(lambda (x) (every #'identity (mapcar #'(lambda (f) (funcall f x)) fns))))

(defun int-div (i div)
  (floor (/ i div)))

(defun int-near (i1 step)
  (abs (- i1 (* step (int-div i1 step)))))

(defun pad-to (list len &key (val 0))
  (let ((llen (length list)))
    (if (< llen len)
	(append list (make-list (- len llen) :initial-element val))
	list)))

(defun index-of (seq elt &key (test #'equal) (key #'identity))
  (loop for i from 0
       for e in seq
       when (funcall test (funcall key e) elt)
       do (return i)
       finally (return nil)))

(defun split-into (data cnt)
  (let ((res (list)))
    (loop with len = (length data)
       for i from 0 upto (1- len) by cnt
       collect (subseq data i (min len (+ i cnt))))))

(defmacro repeat-collect (times &body body)
  (let ((i (gensym)))
    `(loop for ,i below ,times
	  collect (progn ,@body))))

(defun inv-assoc (elt list &key (test #'eql))
  (loop for bla in list
       when (funcall test elt (cdr bla))
       do (return bla)))

(defun substring (str start &optional end)
  (coerce (subseq (coerce str 'list) start end) 'string))

(defun make-keyword (thing)
  (intern (if (symbolp thing)
	      (symbol-name thing)
	      thing) (find-package :keyword)))

(defun copy-store-object-recurse (obj &key hash recurse-classes no-recurse-slots)
  (cond ((and (typep obj 'store-object)
	      (member (class-name (class-of obj))
		      recurse-classes))
	 (copy-store-object obj :hash hash :recursive t
			    :recurse-classes recurse-classes
			    :no-recurse-slots no-recurse-slots))
	((listp obj)
	 (mapcar #'(lambda (x)
		     (copy-store-object-recurse x :hash hash)) obj))
	((hash-table-p obj)
	 (let ((res (make-hash-table :test (hash-table-test obj))))
	   (loop for key being the hash-key of obj
	      for val being the hash-value of obj
		do (setf (gethash key res) (copy-store-object-recurse val :hash hash)))
	   res))
	((stringp obj)
	 obj)
	((arrayp obj)
	 (when (> (length (array-dimensions obj)) 1)
	   (error "can't copy multimensional arrays yet"))
	 (let ((res (make-array (array-dimension obj 0))))
	   (dotimes (i (array-dimension obj 0))
	     (setf (aref res i)
		   (copy-store-object-recurse (aref obj i) :hash hash)))))
	(t obj)))
	   
(defun copy-store-object (obj &key (recursive nil) (hash (make-hash-table :test #'equal))
			  (recurse-classes nil) (no-recurse-slots nil))
  (if (and hash (gethash obj hash))
      obj
      (let* ((class (class-of obj))
	     (res (setf (gethash obj hash) (make-instance (class-name class))))
	     (slots (remove-if #'(lambda (x) (member x
						     '(bknr.indices::destroyed-p bknr.datastore::id)))
			       (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class))))
	     (values (mapcar #'(lambda (slot)
				 (if (slot-boundp obj slot)
				     (let ((value (slot-value obj slot)))
				       ;; (format t "value: ~A slot: ~A~%" value slot)
				       (if (and recursive (not (member slot no-recurse-slots)))
					   (copy-store-object-recurse value :hash hash
								      :recurse-classes recurse-classes
								      :no-recurse-slots no-recurse-slots)
					   value))
				     nil))
			     slots)))
	;; (format t "slots: ~A~%" slots)
	(apply #'change-slot-values res (mapcan #'(lambda (slot value)
						    (when (slot-boundp obj slot)
						      (list slot value)))
						slots values))
	res)))

(defun copy-store-objects (objects &key (recursive nil))
  ;; copy objects modifying linked objects as well
  (let ((hash (make-hash-table :test #'equal)))
    (mapcar #'(lambda (obj)
		(copy-store-object obj :recursive recursive :hash hash))
	    objects)))
