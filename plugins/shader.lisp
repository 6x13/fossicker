(defpackage :fossicker/plugins/shader
  (:use :cxcl)
  (:export #:match-prospect-extension))

(in-package :fossicker/plugins/shader)

(defun match-prospect-extension (ext)
  (cond ((member ext
                 '("vert" "vrt")
                 :test #'string-equal)
         '("vrt" "vert"))
        ((member ext
                 '("frag" "frg")
                 :test #'string-equal)
         '("frag" "frg"))
        ((member ext
                 '("tese" "tes")
                 :test #'string-equal)
         '("tese" "tes"))
        (t nil)))

(in-package :fossicker)

(deflayer shader)

(define-layered-class shader (asset prospect-custom)
  ()
  (:documentation "Generic shader class."))

(defmethod restrict-prospectable-formats ((asset shader))
  (fossicker/plugins/shader:match-prospect-extension
   (pathname-type (slot-value asset 'namestring))))

(define-layered-method dispatch :in shader list (namestring)
  (if (some-regex namestring
                  "\\.vert\$" "\\.frag\$" "\\.tesc\$"
                  "\\.tese\$" "\\.geom\$" "\\.comp\$")
      'shader))

(ensure-active-layer 'shader)
