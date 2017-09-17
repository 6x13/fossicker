;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 20 October 2015

;; This file is part of Fossicker.

;; Fossicker is free  software: you can redistribute it and/or  modify it under
;; the  terms of  the  GNU General  Public  License as  published  by the  Free
;; Software Foundation,  either version 3 of  the License, or (at  your option)
;; any later version.

;; Fossicker is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without  even the implied  warranty of MERCHANTABILITY  or FITNESS
;; FOR  A PARTICULAR  PURPOSE.  See  the GNU  General Public  License for  more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; Fossicker.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

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
