(defpackage :fossicker/plugins/font
  (:use :cxcl))

(in-package :fossicker/plugins/font)

(deflayer font)

(define-layered-class font (asset)
  ()
  (:documentation "Generic font class."))

(define-layered-method dispatch :in font list (namestring)
  '(font . ("\\.ttf\\'" "\\.otf\\'")))
