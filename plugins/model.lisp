(defpackage :fossicker/plugins/model
  (:use :cxcl))

(in-package :fossicker/plugins/model)

(deflayer model)

(define-layered-class model (asset)
  ()
  (:documentation "Generic model class."))

(define-layered-method dispatch :in model list (namestring)
  '(model . ("\\.obj\\'" "\\.3ds\\'")))
