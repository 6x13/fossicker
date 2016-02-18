(defpackage :fossicker/plugins/font
  (:use :cxcl))

(in-package :fossicker)

(deflayer font)

(define-layered-class font (asset prospect-same)
  ()
  (:documentation "Generic font class."))

(define-layered-method dispatch :in font list (namestring)
  (if (some-regex namestring "\\.ttf$" "\\.otf$") 'font))

(ensure-active-layer 'font)
