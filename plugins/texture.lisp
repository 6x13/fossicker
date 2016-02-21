(defpackage :fossicker/plugins/texture
  (:use :cxcl))

(in-package :fossicker)

(deflayer texture)

(defstruct (texture-file (:include file) (:type vector) :named
                         (:constructor make-texture-file
                             (namestring &key status scale)))
  "The TEXTURE-FILE struct."
  (scale (error "Scale needs to be supplied.")
   :type unsigned-byte))

(define-layered-class texture (asset prospect-any)
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
