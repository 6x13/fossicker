(defpackage :fossicker/plugins/shader
  (:use :cxcl))

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

(define-layered-class shader (asset)
  ()
  (:documentation "Generic shader class."))

(define-layered-method dispatch :in shader list (namestring)
  '(shader . ("\\.vert\$" "\\.frag\$" "\\.tesc\$"
              "\\.tese\$" "\\.geom\$" "\\.comp\$")))


