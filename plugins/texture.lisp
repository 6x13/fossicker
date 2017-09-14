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

(defclass texture (prospect-any)
  ((density
    :type list
    :interactive t
    :initarg :density
    :initform nil
    :accessor density
    :documentation "A  list of plists with  :PATH and :SCALE of  exported asset
    files with various densities."))
  (:metaclass resource)
  (:interactive :formats :path :source :files :data :benchmark :density)
  (:documentation "Generic texture class."))

(define-layered-method dispatch :in texture list (rqststring)
  (if (some-regex rqststring "\\.png$" "\\.jpg$" "\\.tiff$" "\\.tga$")
      'texture))

(ensure-active-layer 'texture)
