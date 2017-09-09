(defpackage :fossicker/plugins/inconvenience
  (:use :cl :asdf))

(in-package :fossicker/plugins/inconvenience)

(rename-package "FOSSICKER"
                "FOSSICKER"
                '("FCK"))

(rename-package "FOSSICKER-USER"
                "FOSSICKER-USER"
                '("FCKU"))

(rename-package "FOSSICKER-UI-QT"
                "FOSSICKER-UI-QT"
                '("FCKUIQT"))

(register-system-packages "fossicker"
                          '(:fck :fcku))

(asdf:register-system-packages "fossicker-ui-qt"
							   '(:fckuiqt))
