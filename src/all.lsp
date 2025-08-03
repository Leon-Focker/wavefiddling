;; * all

(require :layers-utils)
(in-package :layers-utils)

;; get path to local files with #'wavefiddling-path
(let ((wavefiddling-src-dir (sc::path-from-same-dir)))
  (defun wavefiddling-path (name)
    (format nil "~a/~a"
	    (sc::parent-dir
	     (sc::parent-dir wavefiddling-src-dir))
	    name)))

(load (wavefiddling-path "src/wavefiddler.lsp"))
(load (wavefiddling-path "src/helpers.lsp"))
(load (wavefiddling-path "src/waves.lsp"))
(load (wavefiddling-path "src/notes.lsp"))
(load (wavefiddling-path "src/compile-sounds.lsp"))

;; EOF all.lsp
