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

