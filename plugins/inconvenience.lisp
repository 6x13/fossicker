(defpackage :fossicker/plugins/inconvenience
  (:use :cl :asdf))

(in-package :fossicker/plugins/inconvenience)

(rename-package "FOSSICKER"
                "FOSSICKER"
                '("FCK"))

(rename-package "FOSSICKER-USER"
                "FOSSICKER-USER"
                '("FCKU"))

(register-system-packages "fossicker"
                          '(:fck :fcku))
