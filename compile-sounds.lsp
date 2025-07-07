;; * compile-sounds

(defmacro compile-sounds (note-list-var
			  image-name
			  folder-name
			  midi-file-name
			  track
			  stretch
			  &body body)
  (let ((wave-list (gensym))
	(name (gensym)))
    `(let* ((,note-list-var (midi-to-notes (wavefiddling-path ,midi-file-name) ,track))
	    (,wave-list '())
	    (,name (pathname-name ,midi-file-name)))
       ;; set timbre-ids for notes
       (loop for note in ,note-list-var
	     with name = (pathname-name ,image-name)
	     do (setf (note-timbre-id note) name))
       
       ;; transform note-list
       ,@body
       
       ;; generate all unique sounds (note-list might have duplicates but
       ;; generate-wave checks with probe-file whether sounds exist)
       (setf ,wave-list (generate-waves ,note-list-var ,image-name ,folder-name ,stretch))
       ;; notes-to-soundfile
       (notes-to-soundfile ,note-list-var ,wave-list ,name))))

#|
(compile-sounds note-list "images/cat.jpg" "test/" "testieren.mid" 1 nil
  (setf note-list (oktaviere note-list)))
|#

;; EOF compile-sounds.lsp
