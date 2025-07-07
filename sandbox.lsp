;; * sandbox

(in-package :ly)

(compile-sounds note-list "images/cat.jpg" "test/" "testieren.mid" 1 nil
  ;;(setf note-list (oktaviere note-list))
  (notes-to-midi (apply-tempo-curve note-list '(0 1  0.5 4  1 1/2)) "/E/code/wavefiddling/tempo.mid"))

;; EOF sandbox.lsp
