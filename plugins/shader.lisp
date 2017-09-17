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

(defclass shader (prospect-custom)
  ()
  (:metaclass resource)
  (:documentation "Generic shader class."))

(defmethod restrict-prospectable-formats ((asset shader))
  (fossicker/plugins/shader:match-prospect-extension
   (pathname-type (slot-value asset 'rqststring))))

(define-layered-method dispatch :in shader list (rqststring)
  (if (some-regex rqststring
                  "\\.vert\$" "\\.frag\$" "\\.tesc\$"
                  "\\.tese\$" "\\.geom\$" "\\.comp\$")
      'shader))

(ensure-active-layer 'shader)
