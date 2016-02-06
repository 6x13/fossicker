(in-package :cl-user)

(defpackage :fossicker/plugins/model
  (:use :cl))

(in-package :fossicker/plugins/model)

(deflayer model)

(define-layered-class model (asset)
  ()
  (:documentation "Generic model class."))

(define-layered-method dispatch :in model list (namestring)
  '(model . ("\\.obj\\'" "\\.3ds\\'")))
