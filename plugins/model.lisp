(defpackage :fossicker/plugins/model
  (:use :cxcl))

(in-package :fossicker)

(deflayer model)

(define-layered-class model (asset prospect-same)
  ()
  (:documentation "Generic model class."))

(define-layered-method dispatch :in model list (rqststring)
  (if (some-regex rqststring "\\.obj$" "\\.3ds$") 'model))

(ensure-active-layer 'model)
