;; * sandbox

(in-package :ly)

(defun compile-sounds (image-name folder-name midi-file-name &optional track stretch)
  (let* ((note-list (midi-to-notes (wavefiddling-path midi-file-name) track))
	 (unique-notes (midi-to-unique-notes (wavefiddling-path midi-file-name) track))
	 (wave-list '())
	 (name (pathname-name midi-file-name)))
    ;; set timbre-ids for notes
    (loop for note in note-list
	  with name = (pathname-name image-name)
	  do (setf (note-timbre-id note) name))
    ;; generate all unique sounds
    (setf wave-list (generate-waves unique-notes image-name folder-name stretch))
    ;; notes-to-soundfile
    (notes-to-soundfile note-list wave-list name)))

(compile-sounds "images/cat.jpg" "test/" "testieren.mid" 1)

;; EOF sandbox.lsp
