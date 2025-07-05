;; * all

(require :layers)
(in-package :layers)

(load "/E/code/wavefiddling/helpers.lsp")

(let ((duration 10)
      (harmony '(1000 1005 a4 cs5 e5)))
  (loop for h in harmony
	do (image-to-pitched-sound "images/cat.jpg" duration h t)))

;; EOF all.lsp
