(in-package :cl-user)

(defpackage :fossicker/plugins/blog
  (:use :fossicker :cl :contextl))

(in-package :fossicker/plugins/blog)

(in-package :fossicker)

(defclass blog (project)
  ((index :initarg :index :initform nil))
  (:documentation "Blog project."))
