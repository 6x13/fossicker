(defpackage :fossicker/plugins/texture
  (:use :cxcl))

(in-package :fossicker)

(deflayer texture)

(define-layered-class texture (asset)
  ((density
    :type list
    :initarg :density
    :initform nil
    :accessor density
    :documentation "A  list of plists with  :PATH and :SCALE of  exported asset
    files with various densities."))
  (:documentation "Generic texture class."))

(define-layered-method dispatch :in texture list (namestring)
  (if (some-regex namestring "\\.png$" "\\.jpg$" "\\.tiff$" "\\.tga$")
      'texture))

(ensure-active-layer 'texture)
