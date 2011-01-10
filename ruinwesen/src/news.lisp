(in-package :ruinwesen)

(define-persistent-class ruinwesen-news (owned-object)
  ((title :update)
   (text :update)
   (date :read :initform (get-universal-time))))