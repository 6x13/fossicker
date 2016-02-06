(defpackage :fossicker/plugins/blog
  (:use :fossicker :cxcl))

(in-package :fossicker/plugins/blog)

(in-package :fossicker)

(defclass blog (project)
  ((index :initarg :index :initform nil))
  (:documentation "Blog project."))
