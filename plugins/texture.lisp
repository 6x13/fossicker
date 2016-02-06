(in-package :cl-user)

(defpackage :fossicker/plugins/texture
  (:use :cxcl))

(in-package :fossicker)

(deflayer texture)

(define-layered-class texture (asset)
  ()
  (:documentation "Generic texture class."))

(define-layered-method dispatch :in texture list (namestring)
  '(texture . ("\\.png$" "\\.jpg$" "\\.tiff$" "\\.tga$")))
