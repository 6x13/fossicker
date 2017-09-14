(defpackage :fossicker/plugins/video
  (:use :cxcl))

(in-package :fossicker)

(deflayer video)

(define-layered-class video (asset prospect-any)
  ()
  (:documentation "Generic video class."))

(define-layered-method dispatch :in video list (rqststring)
  (if (some-regex rqststring "\\.mp4$" "\\.webm$" "\\.flv$")
      'video))

(ensure-active-layer 'video)
