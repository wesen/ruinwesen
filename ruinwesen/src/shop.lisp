(in-package :ruinwesen)

;; Shopping system.

;; The shop offers both downloadable as well as mail-order type
;; products.

;; While browsing the shop web site, customers put products they want
;; to buy into a shopping cart.  The shopping cart is stored a cookie
;; in the browser.

;; Once finished with selecting products, the user is asked to either
;; log in or create a new customer object.  The amount of data that
;; the user is required to supply for the order depends on the type of
;; the order.  To buy download products, only the email address is
;; mandatory.

;; An order object is created in the database from the shopping cart
;; cookie data.


(define-persistent-class product ()
  ((price
    :update
    :type money)
   (delivery-requirements
    :read
    :initform nil)))

(define-persistent-class download-product (product)
  ()
  (:default-initargs
  (:documentation
   "A product that can be directly downloaded from the system.  To buy
    such a product, the customer needs to supply no shipping
    address.  Once paid, the system makes the product available to the
    customer for download."))

(define-persistent-class emailable-product (product)
  ()
  (:documentation
   "A product that can be sent to the customer by email \(i.e. an
    archive product). To buy such a product, an email address needs to
    be supplied by the customer.  Once paid, the system sends the
    order to the store personnel for fulfillment."))

(define-persistent-class mailable-product (product)
  ()
  (:documentation
   "A product that is sent to the customer by regular mail \(i.e. a
    t-shirt or poster).  Once paid, the system sends the order to the
    store personell for fulfillment."))

(define-persistent-class shipping-address ()
  ((country :read))
  (:documentation
   "Abstract base class for shipping addresses.  The child classes of
    SHIPPING-ADDRESS implement a particular surface address structure,
    as required by the country."))

(define-persistent-class customer (user)
  ((name :update)
   (invoice-addresses
    :update
    :documentation
    "List of invoice addresses with the preferred address being the
     CAR of the list.")
   (shipping-addresses
    :update
    :documentation
    "List of shipping addresses with the preferred address being the
     CAR of the list.")))

(define-persistent-class shopping-cart ()
  ((products
    :update
    :initform nil
    :documentation
    "List of products to buy")
   (expires
    :update
    :initform (make-default-shopping-cart-expiration)
    :documentation
    "Universal time at which the shopping cart expires.")))

(defgeneric put-to-shopping-cart (product shopping-cart)
  (:documentation
   "Put PRODUCT into SHOPPING-CART, return SHOPPING-CART"))

(defgeneric remove-from-shopping-cart (product shopping-cart)
  (:documentation
   "Remove PRODUCT from SHOPPING-CART, return PRODUCT or NIL if it had not been in the cart"))

(define-persistent-class order ()
  ((number :read
           :initform (make-order-number))
   (customer :read)
   (items :update)))

(defgeneric make-order (customer shopping-cart)
  (:documentation
   "Create a new ORDER instance, initialized from the CUSTOMER and
    SHOPPING-CART objects supplied.  Returns the order created."))

(define-persistent-class invoice ()
  ((number :read
           :initform (make-invoice-number))
   (items :update)))

