;; * wavefiddler

(in-package :layers)

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

;; EOF wavefiddler.lsp
