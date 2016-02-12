(defpackage :fossicker/plugins/video
  (:use :cxcl))

(in-package :fossicker)

(deflayer video)

(define-layered-class video (asset)
  ()
  (:documentation "Generic video class."))

(define-layered-method dispatch :in video list (namestring)
  (if (some-regex namestring "\\.mp4$" "\\.webm$" "\\.flv$")
      'video))

(ensure-active-layer 'video)
