(in-package :cm)

(defun byte-to-bit (byte &key (bit-count 8))
  (loop for i from (1- bit-count) downto 0
       collect (ldb (byte 1 i) byte)))

(defun bytes-to-bits (bytes &key (bit-count 8))
  (loop for byte in bytes
     appending (byte-to-bit byte :bit-count bit-count)))

(defun bits-to-byte (bits)
  (loop with res = 0
     for i from 0
     for bit in (nreverse bits)
     do (setf (ldb (byte 1 i) res) bit)
       finally (return res)))

(defun pad-bits (bits length)
  (nconc bits (make-list (- length (length bits)) :initial-element 0)))

(defun bits-to-bytes (bits &key (bit-count 8) (pad t))
  (loop for b on bits by #'(lambda (x) (nthcdr bit-count x))
       when (or pad (>= (length b) bit-count))
       collect (bits-to-byte (pad-bits (subseq b 0 (min (length b) bit-count))
				       bit-count))))

(defun convert-bytes (data &key (in-bits 8) (out-bits 7) (pad t))
  (bits-to-bytes (bytes-to-bits data :bit-count in-bits) :bit-count out-bits :pad pad))

(defun sds-8bit-to-7bit (data)
  (convert-bytes data :in-bits 8 :out-bits 7 :pad t))

(defun sds-7bit-to-8bit (data)
  (convert-bytes data :in-bits 7 :out-bits 8 :pad nil))

(defun short-7lsb (short)
  (ldb (byte 0 7) short))

(defun short-7msb (short)
  (ldb (byte 7 7) short))

(defun sds-dump-request (channel sample)
  (list #xf0 #x7e channel #x03 (short-7lsb sample) (short-7msb sample) #xf7))xo

(defun sds-get-request (request)
  (flush *input*)
  (let ((msg (append 