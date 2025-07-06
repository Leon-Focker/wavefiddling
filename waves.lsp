;; * waves

(in-package :layers)

(defstruct wave
  (path "" :type string)                           ; path to soundfile
  (name "" :type string)                           ; name == note-timbre-id
  (duration 0 :type integer)                       ; in ms
  (freq (round (midi-to-freq 60)) :type integer))  ; in Hz

;; ** 'methods'

(defun test-wave-list (wave-list &optional (fn-name "test-wave-list"))
  (unless (listp wave-list)
    (error "~a: wave-list not a list!" fn-name))
  (unless (loop for wave in wave-list always (equal 'wave (type-of wave)))
    (error "~a: not all elements are waves!" fn-name)))

;;; return a wave
(defun parse-wave (path-to-file)
  (let* ((file-name (pathname-name path-to-file))
	 (info (ppcre:split "[_\.\/]" file-name)))
    (make-wave :path path-to-file
	       :name (first info)
	       :duration (parse-integer (second info))
	       :freq (parse-integer (third info)))))

;;; returns a list of waves
(defun parse-waves-in-directory (path-to-dir)
  (let* ((soundfiles (get-sndfiles path-to-dir))
	 (result))
    (loop for sound in soundfiles
	  do (push (parse-wave sound) result))
    result))

(defun format-wave-name (wave)
  (format nil "~a_~a_~a"
	  (wave-name wave)
	  (wave-duration wave)
	  (wave-freq wave)))

(defun generate-wave (image-name duration-in-ms freq
		      &optional stretch (output-name "output/"))
  (let* ((wave (make-wave :name (pathname-name image-name)
			  :duration duration-in-ms
			  :freq freq))
	 (sound-path (format nil "~a~a"
			     (wavefiddling-path output-name)
			     (format-wave-name wave))))
    (setf (wave-path wave) sound-path)
    ;; check whether sound exists, else generate it
    (unless (probe-file sound-path)
      (image-to-pitched-sound
       image-name
       (format-wave-name wave)
       duration-in-ms
       freq
       stretch
       output-name))
    ;; return wave
    wave))

;; EOF waves.lsp
