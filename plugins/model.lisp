(defpackage :fossicker/plugins/model
  (:use :cxcl))

(in-package :fossicker)

(deflayer model)

(define-layered-class model (asset prospect-same)
  ()
  (:documentation "Generic model class."))

(define-layered-method dispatch :in model list (namestring)
  (if (some-regex namestring "\\.obj$" "\\.3ds$") 'model))

(ensure-active-layer 'model)
