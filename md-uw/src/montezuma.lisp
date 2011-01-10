(in-package :md)

;;; mop

(defclass montezuma-persistent-class (persistent-class)
  ())

(defun montezuma-to-string (thing)
  (if (stringp thing)
      thing
      (princ-to-string thing)))

(defmethod montezuma-indexed-slots ((class montezuma-persistent-class))
  (remove-if-not #'(lambda (x)
		     (and (typep x 'montezuma-effective-slot-definition)
			  (slot-value x 'montezuma-index)))
		 (closer-mop:class-slots class)))
	   
(defclass montezuma-direct-slot-definition (bknr.datastore::persistent-direct-slot-definition)
  ((montezuma-index :initarg :montezuma-index :initform nil)
   (montezuma-stored :initarg :montezuma-stored :initform nil)
   (montezuma-convert :initarg :montezuma-convert :initform #'montezuma-to-string)))

(defclass montezuma-effective-slot-definition (bknr.datastore::persistent-effective-slot-definition)
  ((montezuma-index :initarg :montezuma-index :initform nil)
   (montezuma-stored :initarg :montezuma-stored :initform nil)
   (montezuma-convert :initarg :montezuma-convert :initform #'montezuma-to-string)))

(defmethod closer-mop:validate-superclass ((sub montezuma-persistent-class)
				       (super persistent-class))
  t)

(defmethod closer-mop:direct-slot-definition-class ((class montezuma-persistent-class) &key &allow-other-keys)
  'montezuma-direct-slot-definition)

(defmethod closer-mop:effective-slot-definition-class ((class montezuma-persistent-class) &rest initargs)
  (declare (ignore initargs))
  'montezuma-effective-slot-definition)

(defmethod closer-mop:compute-effective-slot-definition :around
    ((class montezuma-persistent-class) name direct-slots)
  (let* ((montezuma-directs (remove-if-not #'(lambda (class)
						(typep class 'montezuma-direct-slot-definition))
					    direct-slots))
	 (convert (car (last (remove nil
				     (mapcar #'(lambda (slot) (slot-value slot 'montezuma-convert))
					     montezuma-directs)))))
	 (index (reduce #'(lambda (&optional x y) (or x y))
			  (mapcar #'(lambda (slot)
				      (slot-value slot 'montezuma-index))
				  montezuma-directs)))
	 (stored (reduce #'(lambda (&optional x y) (or x y))
			  (mapcar #'(lambda (slot)
				      (slot-value slot 'montezuma-stored))
				  montezuma-directs))))
    (let ((normal-slot (call-next-method)))
      (when (typep normal-slot 'montezuma-effective-slot-definition)
	(with-slots (montezuma-index montezuma-stored montezuma-convert) normal-slot
	  (setf montezuma-index index
		montezuma-convert convert
		montezuma-stored stored)))
      normal-slot)))

(defclass montezuma-index ()
  ((index :initarg :index :accessor montezuma-index-index)))

(defmethod initialize-instance :after ((m-index montezuma-index) &key &allow-other-keys)
  (with-slots (index) m-index
    (unless index
      (setf index (make-instance 'montezuma:index
				 :default-search-field "*")))))

(defmethod index-reinitialize ((new-index montezuma-index) (old-index montezuma-index))
  (setf (montezuma-index-index new-index)
	(montezuma-index-index old-index)))

(defun montezuma-object-slots (object)
  (when (typep (class-of object)
	       'montezuma-persistent-class)
;    (mapcar #'(lambda (x) (slot-value x 'sb-pcl::name))
	    (montezuma-indexed-slots (class-of object))))

(defmethod index-add ((index montezuma-index) (obj store-object))
  (let ((doc (make-instance 'montezuma:document)))
    (dolist (slot (montezuma-object-slots obj))
      (let ((slot-name (closer-mop:slot-definition-name slot)))
	(when (slot-boundp obj slot-name)
	  (montezuma:add-field
	   doc
	   (montezuma:make-field (string-downcase (symbol-name slot-name))
				 (funcall (let ((fn (slot-value slot 'montezuma-convert)))
					    (if (symbolp fn)
						(progn
						  (symbol-function fn))
						fn))
					  (slot-value obj slot-name))
				 :stored (slot-value slot 'montezuma-stored)
				 :index (slot-value slot 'montezuma-index))))))
    (montezuma:add-field doc (montezuma:make-field "id" (princ-to-string (store-object-id obj))
					       :stored t :index nil))
    (montezuma:add-document-to-index (montezuma-index-index index) doc)))

(defmethod index-remove ((index montezuma-index) (obj store-object))
  (montezuma:search-each (montezuma-index-index index)
			 (format nil "id:\"~a\"" (store-object-id obj))
			 #'(lambda (doc score)
			     (declare (ignore score))
			     (montezuma:delete-document (montezuma-index-index index) doc))))
      
(defun montezuma-doc-object (index doc)
  (when (numberp doc)
    (setf doc (montezuma:get-document index doc)))
  (store-object-with-id (parse-integer (montezuma:document-value doc "id"))))

(defmethod index-get ((index montezuma-index) key)
  (let ((res))
    (montezuma:search-each (montezuma-index-index index) key
			   #'(lambda (doc score)
			       (push (list (montezuma-doc-object (montezuma-index-index index) doc) score) res)))
    res))
    
(defmethod index-clear ((m-index montezuma-index))
  (with-slots (index) m-index
    (setf index (make-instance 'montezuma:index
			       :default-search-field "*"))))
	  

(defvar *id* 0)

(defun test ()
  (dolist (obj (store-objects-with-class 'machine))
    (let ((doc (make-instance 'montezuma:document)))
      (montezuma:add-field doc (montezuma:make-field "name" (format nil "~a-~A" (symbol-name (machine-name (machine-model obj))) *id*)
						     :stored nil :index :tokenized))
      (montezuma:add-field doc (montezuma:make-field "description" (md-object-description obj)
						      :stored nil :index :tokenized))
      (montezuma:add-field doc (montezuma:make-field "id" (princ-to-string (incf *id*))
						     :stored t :index :tokenized))
      (montezuma:add-document-to-index *index* doc))))

(defparameter *index* (make-instance 'montezuma:index
				     :default-search-field "*"))

(defun add-foo (name body)
  (let ((doc (make-instance 'montezuma:document)))
    (montezuma:add-field doc (montezuma:make-field "name" name
						   :stored nil :index :tokenized))
    (montezuma:add-field doc (montezuma:make-field "body" body
						  :stored nil :index :tokenized))
    (montezuma:add-document-to-index *index* doc)))

(defun search-machine (name)
  (montezuma:search-each *index* name
			 #'(lambda (doc score)
			     (format t "~&doc ~A found with score of ~A."
				     doc score))))


