;; * sandbox

(in-package :ly)

(compile-sounds note-list "images/cat.jpg" "test/" "testieren.mid" 1 nil
  (setf note-list (oktaviere note-list)))

;; EOF sandbox.lsp
