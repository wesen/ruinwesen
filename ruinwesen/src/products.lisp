(in-package :ruinwesen)

(define-persistent-class preorder ()
  ((name :read)
   (email :read)
   (country :read)
   (form-factor :read)
   (device :read)
   (text :read)))

(defun all-rw-preorders ()
  (store-objects-with-class 'preorder))

(defun preorders-to-csv (file &optional (weeks 1))

	(let ((since (- (get-universal-time)
									(* 3600 24 7 weeks))))
	
	(with-open-file (s file :direction :output :external-format :utf-8)
		(format s "\"Name\",\"Email\",\"Country\",\"Command\"~%")
		
		(loop for preorder in (all-rw-preorders)
			 when (> (store-object-last-change preorder 2) since)
			 do (with-slots (name email country device) preorder
						(format s "~S,~S,~S,~S~%"
										name email country device))))))

(define-persistent-class rw-contact ()
  ((name :read)
   (email :read)
   (city :read)
   (text :read)))

(defun all-rw-contacts ()
  (store-objects-with-class 'rw-contact))

(define-persistent-class soundfile (blob)
  ((description :update :initform "")))

(defun foobar (x)
  (+ x 1))

(define-persistent-class product (owned-object)
  ((name :read
	 :index-type string-unique-index
	 :index-reader product-with-name
	 :index-values all-products)
   (price :update
	  :initform (make-money 0 :usd))
   (keywords :update :initform nil
	     :index-type hash-list-index
	     :index-reader get-keyword-products
	     :index-keys all-product-keywords)
   (description :update :initform "")
   (available :update :initform 0)
   (soundfiles :update :initform nil)
   (pictures :update :initform nil)))

