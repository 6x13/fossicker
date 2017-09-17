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

(defpackage :fossicker/plugins/video
  (:use :cxcl))

(in-package :fossicker)

(deflayer video)

(define-layered-class video (asset prospect-any)
  ()
  (:documentation "Generic video class."))

(define-layered-method dispatch :in video list (rqststring)
  (if (some-regex rqststring "\\.mp4$" "\\.webm$" "\\.flv$")
      'video))

(ensure-active-layer 'video)
