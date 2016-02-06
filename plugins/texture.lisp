(in-package :cl-user)

(defpackage :fossicker/plugins/texture
  (:use :cl))

(in-package :fossicker/plugins/texture)

(deflayer texture)

(define-layered-class texture (asset)
  ()
  (:documentation "Generic texture class."))

(define-layered-method dispatch :in texture list (namestring)
  '(texture . ("\\.png$" "\\.jpg$" "\\.tiff$" "\\.tga$")))
