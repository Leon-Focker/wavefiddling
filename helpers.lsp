;; * helpers.lsp

(in-package :layers)

;; ** wavefiddler
  
(defun run-wavefiddler (argument-list &optional path-to-fiddler output)
  (unless (listp argument-list)
    (error "argument-list must be and list but is: ~a" argument-list))
  (cl-user::run-program (or path-to-fiddler
			    #+unix (wavefiddling-path "wavefiddler")
			    #-unix (wavefiddling-path "wavefiddler.exe"))
			argument-list
			:wait t
			:output (or output *standard-output*)))

(defun wavefiddler (source nr-of-frames
		    &key
		      fft-size
		      stretch
		      name
		      help
		      verbose
		      (output "output/"))
  (let ((arguments (list (write-to-string nr-of-frames) "-i"
			 (wavefiddling-path output) "-o"
			 (wavefiddling-path source))))
    (when fft-size
      (push "-f" arguments)
      (push (write-to-string fft-size) arguments))
    (when stretch
      (push "-S" arguments))
    (when name
      (push "-n" arguments)
      (push (wavefiddling-path name) arguments))
    (when help
      (push "-h" arguments))
    (when verbose
      (push "--verbose" arguments))
    (run-wavefiddler (reverse arguments) nil (not verbose))))

;; ** abstractions

(defun image-to-pitched-sound (path-to-image duration-in-ms pitch
			       &optional stretch (output "output/"))
  (let* ((srate 48000)
	 (freq (if (numberp pitch) pitch (note-to-freq pitch)))
	 (fft-size (floor srate freq)))
    (wavefiddler path-to-image (floor (* (/ duration-in-ms 1000) srate) fft-size)
		 :fft-size fft-size
		 :stretch stretch
		 :output output
		 :name (format nil "~a_~a_~a" (pathname-name path-to-image)
			       duration-in-ms
			       freq))))

;; ** CLM

(defmacro wsound (name &body body)
  `(with-sound (:header-type clm::mus-riff :sampling-rate 48000
		:output (format nil "~a~a" (wavefiddling-path ,name) ".wav")
		:channels 2 :play nil :scaled-to 0.98
		:force-recomputation nil)
     ,@body))

(defun parse-wavefiddler-sound-file (file-path)
  (let* ((file-name  (pathname-name file-path))
	 (info (ppcre:split "[_\.\/]" file-name)))
    (list :path file-path
	  :name (first info)
	  :duration (parse-integer (second info))
	  :pitch (parse-integer (third info)))))

;;; returns a list of p-lists. Each p-list contains info for a soundfile within
;;; folder: (path: ... name: ... duration: ... pitch: ...)
;;; duration is in milliseconds, pitch in Hz
(defun parse-wavefiddler-sound-files (folder)
  (let* ((soundfiles (get-sndfiles (wavefiddling-path folder)))
	 (result))
    (loop for sound in soundfiles
	  do (push (parse-wavefiddler-sound-file sound) result))
    result))

(defun midi-to-wavefiddler-score (midi-file &optional track)
  (loop for event in (midi-file-to-list midi-file track)
	collect (list :start (first event)
		      :duration (round (* (third event) 1000)) ; in ms
		      :pitch (floor (midi-to-freq (second event)))
		      :amp (fourth event))))

;;; Example
#|
(compile-sounds "images/cat.jpg" "test/" "testieren.mid" 1)
|#
(defun compile-sounds (path-to-image folder midi-file &optional track)
  (let* ((score (midi-to-wavefiddler-score (wavefiddling-path midi-file) track))
	 (unique-events '())
	 (sounds '()))
    (loop for event in score
	  for ls = (list (getf event :duration) (getf event :pitch))
	  do (unless (find ls unique-events :test 'equal)
	       (push ls unique-events)))
    ;; generate all soundfiles
    (loop for event in unique-events
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
	    with sname = (pathname-name path-to-image)
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
