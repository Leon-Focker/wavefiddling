;; * all

(require :layers-utils)
(in-package :layers-utils)

;; get path to local files with #'wavefiddling-path
(let ((wavefiddling-src-dir (sc::path-from-same-dir)))
  (defun wavefiddling-path (name)
    (format nil "~a~a" wavefiddling-src-dir name)))

(load (wavefiddling-path "wavefiddler.lsp"))
(load (wavefiddling-path "helpers.lsp"))
(load (wavefiddling-path "waves.lsp"))
(load (wavefiddling-path "notes.lsp"))
(load (wavefiddling-path "compile-sounds.lsp"))

;; EOF all.lsp
