(defpackage :fossicker/plugins/particle
  (:use :cxcl))

(in-package :fossicker/plugins/particle)

(deflayer particle)

(define-layered-class particle (asset)
  ()
  (:documentation "Generic particle class."))

(define-layered-method dispatch :in particle list (namestring)
  '(particle . ("\\.pl\\'")))

