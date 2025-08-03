;; * sandbox

(in-package :lyu)

(defparameter *env-1* (midi-to-env (wavefiddling-path "env_1.mid")))
(defparameter *env-2* (midi-to-env (wavefiddling-path "env_2.mid") 1))

(compile-sounds note-list "images/cat.jpg" "test_brit/" "britney.mid" 1 nil
  (setf note-list
	(apply-spectral-envelope
	 (apply-tempo-curve note-list (rescale-env *env-2* 1 2))
	 (rescale-env *env-2* 100 2000)
	 (rescale-env *env-1* 0 1))))

(compile-sounds note-list "images/cat.jpg" "tmp-files/" "midi/duality_60hz_30_30.mid" nil nil
  (setf note-list
	(apply-panning-curve note-list `(0 0  1 ,(* 10 360)) '(0 0  1 0)))
  #+nil(setf note-list
	(apply-spectral-envelope
	 (apply-tempo-curve note-list (rescale-env *env-2* 1 2))
	 (rescale-env *env-2* 100 2000)
	 (rescale-env *env-1* 0 1))))

(compile-sounds note-list "images/lines.jpg" "tmp-files/" "midi/duality_60hz_30_30.mid" nil nil
  (setf note-list
	(apply-panning-curve note-list `(0 0  1 ,(* 10 360)) '(0 180  1 360)))
  #+nil(setf note-list
	(apply-spectral-envelope
	 (apply-tempo-curve note-list (rescale-env *env-2* 1 2))
	 (rescale-env *env-2* 100 2000)
	 (rescale-env *env-1* 0 1))))

;; EOF sandbox.lsp
