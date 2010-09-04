(in-package :portfolio)

(defun find-before (list elt &key (test #'eql))
  (loop for e1 in list
       with prev = nil
       if (funcall test e1 elt)
       return prev
       do (setf prev e1)))

(defun find-after (list elt &key (test #'eql))
  (loop for e1 in list
       with prev = :unbound
       if (funcall test prev elt)
       return e1
       do (setf prev e1)))

(defun make-tags-from-string (string)
  (mapcar #'make-keyword-from-string (cl-ppcre:split "[\\s,]+" string)))  

