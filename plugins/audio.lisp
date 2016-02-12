(defpackage :fossicker/plugins/audio
  (:use :cxcl))

(in-package :fossicker)

(deflayer audio)

(define-layered-class audio (prospect-any asset)
  ()
  (:documentation "Generic audio class."))

(define-layered-method dispatch :in audio list (namestring)
  (if (some-regex namestring
                  "\\.mp3$" "\\.wav$" "\\.ogg$" "\\.aac$")
      'audio))

(ensure-active-layer 'audio)
