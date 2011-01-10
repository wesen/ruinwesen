(define-application-frame hello-world ()
  ((greeting :initform "Hello World"
	     :accessor greeting))
  (:pane (make-pane 'hello-world-pane)))

(defclass hello-world-pane (clim-stream-pane)
  ())

(defmethod handle-repaint ((pane hello-world-pane) region)
  (let ((w (bounding-rectangle-width pane))
	(h (bounding-rectangle-height pane)))
    (draw-rectangle* pane 0 0 w h
		     :filled t
		     :ink (pane-background pane))
    (draw-text* pane
		(greeting *application-frame*)
		(floor w 2) (floor h 2)
		:align-x :center
		:align-y :center)))

(run-frame-top-level
 (make-application-frame 'hello-world))