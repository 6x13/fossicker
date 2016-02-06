(defpackage :fossicker/plugins/audio
  (:use :cxcl))

(in-package :fossicker/plugins/audio)

(deflayer audio)

(define-layered-class audio (asset)
  ()
  (:documentation "Generic audio class."))

(define-layered-method dispatch :in audio list (namestring)
  '(audio . ("\\.mp3\\'" "\\.wav\\'" "\\.ogg\\'" "\\.aac\\'")))
