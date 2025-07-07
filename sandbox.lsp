;; * sandbox

(in-package :ly)

(compile-sounds note-list "images/cat.jpg" "test/" "testieren.mid" 1 nil
  ;;(setf note-list (oktaviere note-list))
  (print (loop for note in note-list collect (note-freq note)))
  (apply-spectral-envelope note-list '(0 200 1 200) '(0 1  1 0))
  (print (loop for note in note-list collect (note-freq note))))

;; EOF sandbox.lsp
