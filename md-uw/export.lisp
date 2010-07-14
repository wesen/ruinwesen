(in-package :md)

;; kit -> write-kit-msg
;; pattern -> write-pattern-msg
;; song -> write-song-msg

(defmethod md-import-objects-of-type ((import md-import) type)
  (remove-if-not #'(lambda (x) (typep x type))
		 (md-import-objects import)))

(defmethod md-import-kits ((import md-import))
  (md-import-objects-of-type import 'kit))

(defmethod md-import-patterns ((import md-import))
  (md-import-objects-of-type import 'md-pattern))

(defmethod md-import-songs ((import md-import))
  (md-import-objects-of-type import 'md-song))

(defmethod md-import-empty-kit-pos ((import md-import))
  (let* ((kits (md-import-kits import))
	 (pos (mapcar #'kit-position kits)))
    (dotimes (i 64)
      (unless (member i pos)
	(return i)))))

(defmethod md-import-empty-song-pos ((import md-import))
  (let* ((songs (md-import-songs import))
	 (pos (mapcar #'md-song-position songs)))
    (dotimes (i 32)
      (unless (member i pos)
	(return i)))))

(defmethod md-import-kit-pos-empty ((import md-import) pos)
  (not (member pos (mapcar #'kit-position (md-import-kits import)))))

(defmethod md-import-pat-pos-empty ((import md-import) pos)
  (not (member pos (mapcar #'md-pattern-position (md-import-patterns import)))))

(defmethod md-import-song-pos-empty ((import md-import) pos)
  (not (member pos (mapcar #'md-song-position (md-import-songs import)))))

(defmethod export-add-objects ((export md-import) objects)
  (let* ((add-objects (copy-store-objects objects))
	 (kits (remove-if-not #'(lambda (x) (typep x 'kit)) add-objects))
	 (patterns (remove-if-not #'(lambda (x) (typep x 'md-pattern)) add-objects))
	 (songs (remove-if-not #'(lambda (x) (typep x 'md-song)) add-objects)))
    (dolist (kit kits)
      (unless (md-import-kit-pos-empty export (kit-position kit))
	(let ((new-pos (md-import-empty-kit-pos export)))
	  (unless new-pos
	    (error "no empty kitposition anymore"))
	  (setf (kit-position kit) new-pos))))
    (dolist (pat patterns)
      (unless (md-import-pat-pos-empty export (md-pattern-position pat))
	(error "already a pattern stored at ~A" (pattern-name (md-pattern-position pat)))))
    (dolist (song songs)
      (unless (md-import-song-pos-empty export (md-song-position song))
	(let ((new-pos (md-import-empty-song-pos export)))
	  (unless new-pos
	    (error "new empty song position anymore"))
	  (setf (md-song-position song) new-pos))))
    (setf (md-import-objects export)
	  (append (md-import-objects export) add-objects))
    export))

(defmethod export-to-file ((export md-import) filename)
  (export-objects (md-import-objects export) filename))

(defun export-objects (objects filename)
    (with-open-file (s filename :direction :output
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
    (dolist (obj objects)
      (my-write-bytes 
       (case (type-of obj)
	 (kit (write-kit-msg obj))
	 (md-pattern (write-pattern-msg obj))
	 (md-song (write-song-msg obj))
	 (t (error "unknown object type ~A" obj))) s))))
