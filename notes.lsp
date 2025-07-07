;; * notes

(in-package :layers)

(defstruct note
  (start 0 :type integer)               ; in miliseconds
  (duration 0 :type integer)            ; in miliseconds
  (freq (midi-to-freq 60) :type number) ; in Hz
  (velocity 0.7 :type number)           ; 0-1
  (timbre-id "" :type string))

;; ** 'methods'

;; *** tests

(defun test-note-list (note-list &optional (fn-name "test-note-list"))
  (unless (listp note-list)
    (error "~a: note-list not a list!" fn-name))
  (unless (loop for note in note-list always (equal 'note (type-of note)))
    (error "~a: not all elements are notes!" fn-name)))

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

;; *** using waves

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
  (loop for note in note-list and i from 0
	with total = (length note-list)
	with progress = 0
	do (let ((new-pro (round (* (/ i total) 100))))
	     (unless (= new-pro progress)
	       (setf progress new-pro)
	       (format t "~&Generating waves: ~a%" progress)))
	collect (generate-wave
		 image-name
		 (note-duration note)
		 (note-freq note)
		 stretch
		 folder-name)))

;; *** transform

(defun oktaviere (note-list)
  (test-note-list note-list "oktaviere")
  ;; copy all events with freq * 2
  (loop for note in note-list
	for copy = (copy-structure note)
	do (setf (note-freq copy) (* (note-freq copy) 2))
	collect note
	collect copy))

(defun multiply-spectrum (note-list factor)
  (test-note-list note-list "multiply-spectrum")
  (loop for note in note-list
	do (setf (note-freq note) (* (note-freq note) factor))
	collect note))

(defun shift-spectrum (note-list offset)
  (test-note-list note-list "shift-spectrum")
  (loop for note in note-list
	do (setf (note-freq note) (+ (note-freq note) offset))
	collect note))

(defun sort-note-list (note-list)
  (sort note-list #'(lambda (x y) (< (note-start x) (note-start y)))))

;;; centroid-env is an envelope, where the y-values define the spectral centre
;;; for all notes at the time of x. The x-values are mapped to range from the
;;; first to the last note of note-list.
;;; spread-env is an envelope, where the y-values define the width of the band
;;; around the centroid that notes can cover. Should be between 0 and 1.
;;; 0 means that only the centroid frequency is alowed, 1 means all frequencies
;;; stay unchanged. Something in between shifts all notes toward the centroid.
(defun apply-spectral-envelope (note-list centroid-env spread-env)
  (test-note-list note-list "apply-spectral-envelope")
  (let* ((centroid-first-x (first centroid-env))
	 (centroid-last-x (lastx centroid-env))
	 (spread-first-x (first spread-env))
	 (spread-last-x (lastx spread-env))
	 first
	 last
	 lowest
	 highest)
    ;; get first and last start-time, and lowest and highest freq 
    (loop for note in note-list
	  for freq = (note-freq note)
	  for start = (note-start note)
	  minimize start into smin
	  maximize start into smax
	  minimize freq into min
	  maximize freq into max
	  finally (setf first smin
			last smax
			lowest min
			highest max))
    ;; set all new freqs
    (loop for note in note-list
	  for start = (note-start note)
	  for centroid-normalized-pos
	    = (rescale start first last centroid-first-x centroid-last-x)
	  for spread-normalized-pos
	    = (rescale start first last spread-first-x spread-last-x)
	  for centroid = (interpolate centroid-normalized-pos centroid-env)
	  for spread = (interpolate spread-normalized-pos spread-env)
	  for new-min = (- centroid (* (- centroid lowest) spread))
	  for new-max = (+ centroid (* (- highest centroid) spread))
	  for new-freq
	    = (if (= spread 0)
		  (round centroid)
		  (abs (round (rescale
			       (note-freq note)
			       lowest
			       highest
			       new-min
			       new-max
			       #'warn))))
	  do (setf (note-freq note) new-freq)
	  collect note)))

;;; tempo-mult-env is an envelope, where the y-values are a multiplier for the
;;; tempo of a note. The x-values are mapped to range from the first to the last
;;; note of the note-list.
(defun apply-tempo-curve (note-list tempo-mult-env)
  (test-note-list note-list "apply-tempo-curve")
  (let* ((sorted-notes (sort-note-list note-list))
	 (first (note-start (first sorted-notes)))
	 (last (note-start (car (last sorted-notes))))
	 (env-first (first tempo-mult-env))
	 (env-last (lastx tempo-mult-env)))
    ;; clone first note
    (push (copy-structure (first sorted-notes)) sorted-notes)
    ;; loop through all notes and set new scaled starts and duration
    (loop for last-note in sorted-notes and note in (cdr sorted-notes)
	  with last-old-start = 0	  
	  for old-start = (note-start note)
	  for dur-since-last-old-start = (- (note-start note) last-old-start)
	  for normalized-pos = (rescale old-start first last env-first env-last)
	  for multiplier = (interpolate normalized-pos tempo-mult-env)
	  for new-dur-since-last-old-start
	    = (round (/ dur-since-last-old-start multiplier))
	  for duration-ratio
	    = (/ (note-duration last-note)
		 (if (= 0 dur-since-last-old-start)
		     ;; add small error to avoid / 0 in chords
		     (1+ dur-since-last-old-start) 
		     dur-since-last-old-start))
	  for new-duration = (round (* duration-ratio new-dur-since-last-old-start))
	  do (setf (note-start note)
		   (+ (note-start last-note) new-dur-since-last-old-start)
		   (note-duration last-note)
		   new-duration
		   last-old-start old-start))
    ;; return note-list
    (cdr sorted-notes)))

;; change sounds depending on stats

;; EOF notes.lsp
