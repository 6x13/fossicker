;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 23 October 2015

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

;;;;;;;;;;;;;;
;;; Dispatcher
;;
;;

(defun some-regex (namestring &rest regex)
  "Check if NAMESTRING matches any of the REGEX strings."
  ;; Fix for performance reading CL-PPCRE docs.
  (some
   (lambda (rx)
     (scan (create-scanner rx :case-insensitive-mode t)
           namestring))
   regex))

(define-layered-function dispatch (namestring)
  (:method-combination list :most-specific-last)
  (:documentation "Dispatch  loaded asset  subclasses on  NAMESTRING.  Whatever
  CLASS-NAMEs the  active dispatch methods  return will be listed  as potential
  dispatch targets. If the DISPATCH method returns NIL, it is removed from list
  by the :AROUND method.")
  (:method list (namestring)
    "Returns  NIL, which  is  going to  be  removed from  list  by the  :AROUND
    method. There needs to be at least one primary method." nil)
  (:method :around (namestring)
    "Removes NIL elements from the dispatch list."
    (remove nil (call-next-method))))

;;
;;;; Asset
;;
;;

(define-layered-class asset ()
  ((formats
    :type (or null t list)
    :allocation :class
    :documentation "List  of possible file  formats that  can be used  as asset
    source.")
   (path
    :type (or null string)
    :initarg :path
    :documentation "Path of asset relative to project path.")
   (namestring
    :type string
    :initarg :namestring
    :initform (error "Asset doesn't have a namestring.")
    :accessor asset-namestring
    :documentation "Namestring that is provided by user for generated asset.")
   (source
    :type pathname
    :initarg :source
    :accessor asset-source
    :documentation "Currently selected asset source.")
   (files
    :type list
    :documentation  "List of  FILE class  instances that  are generated  by the
    asset.")
   (date
    :type integer
    :initarg :date
    :initform (get-universal-time)
    :documentation "Date of creation for ASSET instance.")
   (description
    :type string
    :documentation "Description of the asset to be used in generated report.")
   (benchmark
    :type string
    :documentation  "The  data  reported  by TIME  macro  for  measuring  asset
    processor performance."))
  (:documentation "The asset class."))

(defgeneric save (asset)
  (:documentation "Saves asset and creates the corresponding file[s] on project
  path. Default method  simply ignores the input and does  nothing.  Each asset
  subclass should implement their own export method.")
  (:method ((asset asset))))

