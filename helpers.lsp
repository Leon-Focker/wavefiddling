;; * helpers.lsp

(in-package :layers)

;; ** CLM

(defmacro wsound (name &body body)
  `(with-sound (:header-type clm::mus-riff :sampling-rate 48000
		:output (format nil "~a~a" (wavefiddling-path ,name) ".wav")
		:channels 2 :play nil :scaled-to 0.98
		:force-recomputation nil)
     ,@body))


;;; Example
#|
(compile-sounds "images/cat.jpg" "test/" "testieren.mid" 1)
|#
#+nil(defun compile-sounds (path-to-image folder midi-file &optional track)
  (let* ((score (midi-to-wavefiddler-score (wavefiddling-path midi-file) track))
	 (unique-events '())
	 (sounds '())
	 (sname (pathname-name path-to-image)))
    ;; collect unique events
    (loop for event in score
	  for ls = (list (getf event :duration) (getf event :pitch))
	  do (unless (find ls unique-events :test 'equal)
	       (push ls unique-events)))
    ;; check whether there already are any soundfiles in the folder
    (when (probe-file (wavefiddling-path folder))
      (setf sounds (parse-wavefiddler-sound-files folder)))
    ;; generate all soundfiles
    (loop for event in unique-events
	  unless (loop for sound in sounds
		       thereis (and (= (getf sound :duration)
				       (first event))
				    (= (getf sound :pitch)
				       (second event))
				    (equal (getf sound :name)
					   sname)))
	    do (image-to-pitched-sound
		path-to-image
		(first event)
		(second event)
		nil
		folder))
    ;; wait for soundfile generation
    ;;    (sleep 5)
    ;; get all soundfiles from the folder and read their properties
    ;; this is a list of p-lists:  (path: ... name: ... duration: ... pitch: ...)
    (setf sounds (parse-wavefiddler-sound-files folder))
    (when (= (length sounds) 0)
      (error "no soundfiles in ~a" folder))
    ;; generate one compiled soundfile:
    (wsound
	(pathname-name midi-file)
      (loop for event in score
	    for path = (loop for sound in sounds
			     when (and (= (getf sound :duration)
					  (getf event :duration))
				       (= (getf sound :pitch)
					  (getf event :pitch))
				       (equal (getf sound :name)
					      sname))				      
			       return (getf sound :path))
	    do (if path
		   (samp0 path
			  (getf event :start)
			  :amp (getf event :amp))
		   (warn "soundfile not found!"))))))

;; EOF helpers.lsp
