(in-package :md)

;;; portmidi stuff

(defvar *input*)
(defvar *output*)

#+nil
(defun open-pm ()
  (setf *input* (pm:openinput 1 512))
  (setf *output* (pm:openoutput 3 512 0)))

(defun read-sysex (in)
  (let* ((buf (pm:eventbuffernew 100)))
    (unwind-protect
	 (let ((res (list))
	       (finished nil))
	 (loop when finished
	    do (return (nreverse res))

	    when (not (pm:poll in))
	    do (progn (sleep 0.2)
		      #+nil(format t "sleep~%"))
	    else
	    do (let ((num (pm:read in buf 100)))
		 (dotimes (i num)
		   (let ((data (m-to-sysex (pm::event.message (pm::eventbufferelt buf i)))))
;;		     (format t "data: ~A~%" data)
		     (cond ((eq data :EOS)
			    (setf finished t)
			    (return))
			   ((not (null data))
			    (dolist (i data)
			      (if (eq i :EOS)
				  (progn (setf finished t)
					 (return))
				  (push i res))))))))))
    (pm::eventbufferfree buf))))

#-nil
(defun write-sysex (out arr)
  (pm:writesysex out 0
		 (babel:octets-to-string
		  (coerce arr '(vector (unsigned-byte 8)))
		  :external-format :iso-8859-1)))

#-nil
(defun flush (in)
  (let* ((buf (pm:eventbuffernew 1000)))
    (unwind-protect
	 (loop while (> (pm:read in buf 1000) 0))
      (pm:eventbufferfree buf))))

#-nil
(defun message-bytes (m)
  (list (pm:message.status m)
	(pm:message.data1 m)
	(pm:message.data2 m)))

(defun eox-byte-p (byte)
  (= byte #xf7))

(defun end-sysex-p (m)
  (or (non-rt-status-p m)
      (some #'eox-byte-p (message-bytes m))))

#-nil
(defun rt-status-p (m)
  (> (pm:message.status m) #xF7))

(defun sysex-start-p (m)
  (= (pm:message.status m) #xf0))

(defun non-rt-status-p (m)
  (and (> (pm:message.status m) 127)
       (not (sysex-start-p m))
       (not (end-sysex-p m))
       (< (pm:message.status m) #xF8)))

(defun m-to-sysex (m)
  (cond ((non-rt-status-p m) :EOS)
	((rt-status-p m) nil)
	(t (mapcan #'(lambda (x)
			(if (= x #xf7)
			    (list #xf7 :EOS)
			    (list x)))
		(message-bytes m)))))

(defun read-sysex-bytes (s len)
  (sysex-to-data (get-bytes s (+ len (ceiling (/ len 7))))))


;;; buf-arr stuff

(defclass buf-arr ()
  ((arr :initarg :arr
	:accessor buf-arr-arr)
   (checksum :initform 0
	     :accessor buf-arr-checksum)
   (calc-checksum :initform nil
		  :accessor buf-arr-calc-checksum)))

(defun start-checksum (s)
  (when (typep s 'buf-arr)
    (setf (buf-arr-calc-checksum s) t)))

(defun stop-checksum (s)
  (when (typep s 'buf-arr)
    (setf (buf-arr-calc-checksum s) nil)))

(defun put-byte (byte s)
  (cond ((typep s 'buf-arr)
	 (when (buf-arr-calc-checksum s)
	   (incf (buf-arr-checksum s) byte))
	 (push byte (buf-arr-arr s)))
	(t (write-byte byte s))))

(defun get-checksum (s)
  (short-to-sysex
   (if (typep s 'buf-arr)
       (buf-arr-checksum s)
       0)))

(defun get-length (s)
  (if (typep s 'buf-arr)
      (length (buf-arr-arr s))
      0))

(defun get-byte (s)
  (cond ((typep s 'buf-arr)
	 (pop (buf-arr-arr s)))
	(t (read-byte s))))

(defun my-write-bytes (bytes s)
  (dolist (byte bytes)
    (write-byte byte s)))

(defmacro with-get-byte-from-arr ((s arr) &body body)
  `(let ((,s (make-instance 'buf-arr :arr ,arr)))
	 ,@body))

(defmacro with-put-byte-to-list ((s) &body body)
  `(let ((,s (make-instance 'buf-arr :arr (list))))
     ,@body
     (nreverse (buf-arr-arr ,s))))

(defun get-bytes (s len)
  (loop for i from 0 upto (1- len)
       collect (get-byte s)))

(defun put-bytes (bytes s)
  (loop for byte in bytes
     do (put-byte byte s)))

;;; parser / gen stuff

(defun short-to-sysex (short)
  (list (ldb (byte 7 7) short)
	(ldb (byte 7 0) short)))

(defun hsb (byte)
  (ldb (byte 1 7) byte))

(defun lrsb (byte)
  (ldb (byte 7 0) byte))

(defun make-c-string (bytes)
  (setf bytes (subst 0 127 bytes))
  (babel:octets-to-string (coerce bytes '(vector (unsigned-byte 8)))
			   :external-format :iso-8859-1 :end (index-of bytes 0)))

(defun string-to-sysex (string)
  (pad-to (coerce
	   (babel:string-to-octets (if (> (Length string) 16)
					(substring string 0 16)
				       string) :external-format :iso-8859-1)
	   'list) 16))

(defun make-byte (bits)
  (let ((res 0))
    (loop for bit in bits
	 for i from 0 
	 do (setf (ldb (byte 1 i) res)  bit))
    res))

(defun make-long (bytes)
  (let ((res 0))
    (loop for i from 0
	 for byte in (reverse bytes)
	 do (setf (ldb (byte 8 (* i 8)) res) byte))
    res))

(defun make-long-le (bytes)
  (let ((res 0))
    (loop for i from 0
	 for byte in bytes
	 do (setf (ldb (byte 8 (* i 8)) res) byte))
    res))

(defun long-to-bytes (long)
  (list (ldb (byte 8 24) long)
	(ldb (byte 8 16) long)
	(ldb (byte 8 8) long)
	(ldb (byte 8 0) long)))

(defun short-to-bytes (long)
  (list 
	(ldb (byte 8 8) long)
	(ldb (byte 8 0) long)))


(defun make-sysex-block (data)
  (cons (make-byte (nreverse (pad-to (mapcar #'hsb data) 7)))
	(mapcar #'lrsb data)))

;; convert 8 bit to 7 bit
(defun data-to-sysex (data)
  (let ((res (list)))
    (loop for block in (split-into data 7)
       nconc (make-sysex-block block))))

(defun get-bit (byte i)
  (ldb (byte 1 i) byte))

(defun split-byte (byte)
  (loop for i from 0 upto 7
       collect (get-bit byte i)))

(defun split-7bit (byte)
  (loop for i from 0 upto 6
       collect (get-bit byte i)))

(defun make-byte-2 (hsb lrsb)
  (setf (ldb (byte 1 7) lrsb) hsb)
  lrsb)

(defun sysex-block-to-data (block)
  (mapcar #'make-byte-2 (nreverse (split-7bit (first block))) (cdr block)))

(defun sysex-to-data (sysex)
  (loop for block in (split-into sysex 8)
     nconc (sysex-block-to-data block)))
