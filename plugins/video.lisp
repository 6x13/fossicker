(in-package :cl-user)

(defpackage :fossicker/plugins/video
  (:use :cl))

(in-package :fossicker/plugins/video)

(deflayer video)

(define-layered-class video (asset)
  ()
  (:documentation "Generic video class."))

(define-layered-method dispatch :in video list (namestring)
  '(video . ("\\.mp4\\'" "\\.webm\\'" "\\.flv\\'")))
