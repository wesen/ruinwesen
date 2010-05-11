(in-package :ruinwesen)

(defun make-money (amount &optional (currency :eur))
  (list 'money  currency amount))

(defun amount (money)
  (third money))

(defun currency (money)
  (second money))
