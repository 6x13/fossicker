(in-package :cl-user)

(defpackage :fossicker/plugins/data
  (:use :cxcl))

(in-package :fossicker/plugins/data)

(deflayer data)

(define-layered-class data (asset)
  ()
  (:documentation "Generic data class."))

(define-layered-method dispatch :in data list (namestring)
  '(data . ("\\.xml\\'" "\\.json\\'")))

