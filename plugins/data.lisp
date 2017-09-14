(defpackage :fossicker/plugins/data
  (:use :cxcl))

(in-package :fossicker)

(deflayer data)

(define-layered-class data (asset prospect-same)
  ()
  (:documentation "Generic data class."))

(define-layered-method dispatch :in data list (rqststring)
  (if (some-regex rqststring "\\.xml$" "\\.json$") 'data))

(ensure-active-layer 'data)
