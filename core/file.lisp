;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 7 February 2016

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

;;;;;;;;
;;; File
;;
;;

(define-layered-class file ()
  ((path
    :type pathname
    :initarg :path
    :accessor path
    :documentation "Pathname object pointing to the stored file.")
   (checksum
    :type (or null string)
    :initarg :checksum
    :initform nil
    :accessor checksum
    :documentation "MD5  checksum of  the generated file.   User chose  to omit
    writing the file if CHECKSUM is NIL.")
   (date
    :type integer
    :initarg :date
    :initform (get-universal-time)
    :documentation "Date file created.")
   (benchmark
    :type string
    :accessor benchmark
    :documentation  "The  data  reported  by TIME  macro  for  measuring  asset
    processor performance."))
  (:documentation "The file class."))

(defgeneric md5sum (file)
  (:documentation  "Calculates  the   MD5  checksum  of  file   to  check  file
  modifications in the future.")
  (:method ((file file))
    (setf (checksum file) (ironclad:byte-array-to-hex-string
                           (ironclad:digest-file :md5 (path file))))))

(defmethod initialize-instance :after ((instance file) &key)
  (when (or (not (probe-file (path instance)))
            (prompt "File exists at location ~A. Sure you want to overwrite?"
                    (namestring (path instance))))
    ;; TODO: Call function to generate file.
    (setf (checksum instance) (md5sum instance))))

(defgeneric browse (file)
  (:documentation  "Browse the file using the web browser.")
  (:method ((file file)) nil))
