(defpackage :fossicker/plugins/data
  (:use :cxcl))

(in-package :fossicker)

(deflayer data)

(define-layered-class data (prospect-same asset)
  ()
  (:documentation "Generic data class."))

(define-layered-method dispatch :in data list (namestring)
  (if (some-regex namestring "\\.xml$" "\\.json$") 'data))

(ensure-active-layer 'data)
