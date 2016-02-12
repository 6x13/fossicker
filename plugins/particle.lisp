(defpackage :fossicker/plugins/particle
  (:use :cxcl))

(in-package :fossicker)

(deflayer particle)

(define-layered-class particle (asset)
  ()
  (:documentation "Generic particle class."))

(define-layered-method dispatch :in particle list (namestring)
  (if (some-regex namestring "\\.pl$") 'particle))

(ensure-active-layer 'particle)
