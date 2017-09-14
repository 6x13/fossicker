(defpackage :fossicker/plugins/particle
  (:use :cxcl))

(in-package :fossicker)

(deflayer particle)

(define-layered-class particle (asset prospect-same)
  ()
  (:documentation "Generic particle class."))

(define-layered-method dispatch :in particle list (rqststring)
  (if (some-regex rqststring "\\.pl$") 'particle))

(ensure-active-layer 'particle)
