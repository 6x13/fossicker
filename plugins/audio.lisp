(defpackage :fossicker/plugins/audio
  (:use :cxcl))

(in-package :fossicker)

(deflayer audio)

(defclass audio (prospect-any)
  ()
  (:metaclass resource)
  (:documentation "Generic audio class."))

(define-layered-method dispatch :in audio list (rqststring)
  (if (some-regex rqststring
                  "\\.mp3$" "\\.wav$" "\\.ogg$" "\\.aac$")
      'audio))

(ensure-active-layer 'audio)
