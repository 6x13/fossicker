;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 4 February 2016

;; This file is part of Fossicker.

;; Fossicker is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Fossicker is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Fossicker.  If not, see <http://www.gnu.org/licenses/>.

(in-package :fossicker)

;; (declaim (optimize                                                       
;;           (speed 0) (compilation-speed 0) (safety 3) (debug 3)))

;;;;;;;;;;
;;; Reader
;;
;;

;;;;;;;;;
;;; Class
;;
;;

;;;;;;;;;;;;;;;;;;;
;;; Store & Restore
;;
;;

;; (defgeneric store (project)
;;   (:documentation
;;    "Generates asset, pushes it to ASSETS and sets it as CURRENT.")
;;   (:method ((project project)))
;;   (store (merge-pathnames* (project-file-directory project) "") ()))
