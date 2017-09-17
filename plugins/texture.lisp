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

(defpackage :fossicker/plugins/texture
  (:use :cxcl))

(in-package :fossicker)

(deflayer texture)

(defstruct (texture-file (:include file) (:type vector) :named
                         (:constructor make-texture-file
                             (namestring &key status scale)))
  "The TEXTURE-FILE struct."
  (scale (error "Scale needs to be supplied.")
   :type unsigned-byte))

(defclass texture (prospect-any)
  ((density
    :type list
    :interactive t
    :initarg :density
    :initform nil
    :accessor density
    :documentation "A  list of plists with  :PATH and :SCALE of  exported asset
    files with various densities."))
  (:metaclass resource)
  (:interactive :formats :path :source :files :data :benchmark :density)
  (:documentation "Generic texture class."))

(define-layered-method dispatch :in texture list (rqststring)
  (if (some-regex rqststring "\\.png$" "\\.jpg$" "\\.tiff$" "\\.tga$")
      'texture))

(ensure-active-layer 'texture)
