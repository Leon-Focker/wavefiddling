;; * notes

(in-package :layers)

(defstruct note
  (start 0 :type number)
  (duration 1 :type number)
  (freq (midi-to-freq 60) :type number)
  (velocity 0.7 :type number)
  (timbre-id "" :type string))

;; ** 'methods'

(defun test-note-list (note-list &optional (fn-name "test-note-list"))
  (unless (listp note-list)
    (error "~a: note-list not a list!" fn-name))
  (unless (loop for note in note-list always (equal 'note (type-of note)))
    (error "~a: not all elements are notes!" fn-name)))

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
	  do (push (note-duration note) durs)
	  do (push (note-start note) starts)
	  do (push (note-velocity note) velos))
    (lists-to-midi pitches durs starts
		   :velocity-list velos
		   :file file)))

(defun midi-to-notes (midi-file &optional track)
  (loop for event in (midi-file-to-list midi-file track)
	collect (make-note :start (first event)
			   :duration (round (* (third event) 1000)) ; in ms
			   :freq (floor (midi-to-freq (second event)))
			   :velocity (fourth event))))

;; EOF notes.lsp
