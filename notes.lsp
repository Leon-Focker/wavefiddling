;; * notes

(in-package :layers)

(defstruct note
  (start 0 :type integer)               ; in miliseconds
  (duration 0 :type integer)            ; in miliseconds
  (freq (midi-to-freq 60) :type number) ; in Hz
  (velocity 0.7 :type number)           ; 0-1
  (timbre-id "" :type string))

;; ** 'methods'

(defun test-note-list (note-list &optional (fn-name "test-note-list"))
  (unless (listp note-list)
    (error "~a: note-list not a list!" fn-name))
  (unless (loop for note in note-list always (equal 'note (type-of note)))
    (error "~a: not all elements are notes!" fn-name)))

;;; TODO might be better to have a #'multiply-freq function?
(defun oktaviere (note-list)
  (test-note-list note-list "oktaviere")
  ;; copy all events with freq * 2
  (loop for note in note-list
	for copy = (copy-structure note)
	do (setf (note-freq copy) (* (note-freq copy) 2))
	collect note
	collect copy))

;; *** midi

(defun notes-to-midi (note-list file)
  (test-note-list note-list "notes-to-midi")
  (let ((pitches '())
	(durs '())
	(starts '())
	(velos '()))
    (loop for note in note-list
	  do (push (freq-to-midi (note-freq note)) pitches)
	  do (push (/ (note-duration note) 1000) durs)
	  do (push (/ (note-start note) 1000) starts)
	  do (push (note-velocity note) velos))
    (lists-to-midi pitches durs starts
		   :velocity-list velos
		   :file file)))

(defun midi-to-notes (midi-file &optional track)
  (loop for event in (midi-file-to-list midi-file track)
	collect (make-note :start (round (* (first event) 1000))    ; in ms
			   :duration (round (* (third event) 1000)) ; in ms
			   :freq (floor (midi-to-freq (second event)))
			   :velocity (fourth event))))

;;; get a wave-list with unique pairs of duration and frequency from midi notes
(defun midi-to-unique-notes (midi-file &optional track)
  (let ((unique-events '()))
    ;; get unique events (pairs of duration and freq)
    (loop for event in (midi-file-to-list midi-file track)
	  ;; list of duration in ms and freq in Hz
	  for ls = (list (round (* (third event) 1000))
			 (floor (midi-to-freq (second event))))
	  do (unless (find ls unique-events :test 'equal)
	       (push ls unique-events)))
    ;; make notes
    (loop for event in unique-events
	  collect (make-note :duration (first event)
			     :freq (second event)))))

;;; find a wave that fits the info from note within a wave-list. If none is
;;; found, return nil.
(defun find-wave-with-note (wave-list note)
  (test-wave-list wave-list "find-wave-with-note")
  (loop for wave in wave-list
	when (and (= (wave-duration wave)
		     (note-duration note))
		  (= (wave-freq wave)
		     (note-freq note))
		  (equal (wave-name wave)
			 (note-timbre-id note)))
	  return wave))

(defun notes-to-soundfile (note-list
			   wave-list
			   &optional (file-name "soundfile"))
  ;; sanity checks
  (test-note-list note-list "notes-to-soundfile")
  (test-wave-list wave-list "notes-to-soundfile")
  ;; call to clm and samp0
  (wsound file-name
    (loop for note in note-list
	  for wave = (find-wave-with-note wave-list note)
	  for path = (when wave (wave-path wave))
	  do (if (print path)
		 (samp0 path
			(/ (note-start note) 1000.0)
			:amp (note-velocity note))
		 (warn "soundfile not found!")))))

;;; generate all soundfiles needed from a list of notes into a folder.
;;; return a list of waves
(defun generate-waves (note-list image-name folder-name &optional stretch)
  (test-note-list note-list "generate-waves")
  (loop for note in note-list
	collect (generate-wave
		 image-name
		 (note-duration note)
		 (note-freq note)
		 stretch
		 folder-name)))

;; EOF notes.lsp
