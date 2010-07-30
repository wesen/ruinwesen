(in-package :template)

(define-persistent-class portfolio-project ()
  ((title :update
          :index-type string-unique-index
          :index-reader get-project-with-title)
   (tags :update :initform nil
         :index-type hash-list-index
         :index-reader get-projects-with-tag
         :index-keys get-project-tags)
   (time :update :initform (get-universal-time))
   (description :update :initform "")
   (image :read :initform nil)
   (box-image :read :initform nil)))

(defun project-tag-count (tag)
  (length (get-projects-with-tag tag)))

(defun project-tag-sorted-list ()
  (let* ((tags (get-project-tags))
         (count-list (mapcar #'(lambda (tag) (cons tag (project-tag-count tag))) tags)))
    (sort count-list #'> :key #'cdr)))

(defun get-projects-with-tag-sorted (tag)
  (when (stringp tag)
    (setf tag (make-keyword-from-string tag)))
  (let ((projects (get-projects-with-tag tag)))
    (sort (copy-tree projects) #'> :key #'portfolio-project-time)))

#|
(make-instance 'portfolio-project
          :title "AVR-SYNTH"
          :tags '(:avr :microcontroller :cnc :programming :dsp :music :synthesizer :midi :C :assembler :pcb)
          :description "While foraging into electronics, I built a small 4-voice digital/analog synthesizer built upon the AVR microcontroller. Using hand-optimized assembler routines, I was able to generate 2 LFOs and 4 polyphonic voices (square, triangle, noise and \"sine\" waveforms), with an inbuilt sequencer and MIDI control. The voices are generated using a \"noisy\" discrete R2R-DAC. A PWM low-pass filtered CV voltage can be used to control the frequency of an analog Sallen-Key filter. While being quite rough without filter, the synthesizer is very soft and \"deep\" when used in combination with the analog filter. It can produce sounds ranging from 8-bit gritty atari sounds, over to huge deep basses, soft dub chords, and short clavinet stabs."
          :image (store-object-with-id 1)
          :box-image (store-object-with-id 2)
          :time (cybertiggyr-time:parse-time "2008-01-01"))
|#


(defun slurp-file (file)
  (chunga:trim-whitespace
   (with-output-to-string (out)
     (with-open-file (s file)
       (loop for line = (read-line s nil 'foo)
          until (eq line 'foo)
          do (princ line out))))))

(defun import-projects ()
  (loop for file in (directory "/home/manuel/portfolio-images/txts/*.txt")
     for filename = (pathname-name file)
       for tag-file = (make-pathname :defaults #p"/home/manuel/portfolio-images/txts/tags/bla.txt"
                                     :name filename)
     for image-file = (make-pathname :defaults #p"/home/manuel/portfolio-images/full/bla.png"
                                     :name filename)
       for time-file = (make-pathname :defaults #p"/home/manuel/portfolio-images/txts/times/bla.txt"
                                      :name filename)
       for time = (cybertiggyr-time:parse-time (slurp-file time-file))
       for boximage-file = (make-pathname :defaults #p"/home/manuel/portfolio-images/boxes/bla.png"
                                        :name (format nil "~a-box" filename))
       for description = (slurp-file file)
       for title = (string-upcase filename)
       for tags = (remove :|| (remove :|TAGS:| (make-tags-from-string (slurp-file tag-file))))
       unless (not (null (get-project-with-title title)))
     collect (let ((image (import-image image-file :name title))
                   (box-image (import-image boximage-file :name (format nil "~A-BOX" title))))
               (make-instance 'portfolio-project :title title :tags tags :description description :time time :image image :box-image box-image))))