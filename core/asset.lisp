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

;;;;;;;;;;
;;; Assets
;;
;;

(defvar *dispatch-table* nil)

;;
;;;; Register Asset
;;
;;

(defun ignore-function (&rest args)
  "Default argument for register-type FUNCTION parameter."
  nil)

(defun register-asset (name
                       override
                       &key
                         regexp
                         (function #'ignore-function)
                         formats)
  "Register a new fossicker type. 
Fossicker ASSET is determined according a :REGEXP,
usually matching file extensions.
The asset picked from fossicker data path is
processed by :FUNCTION in order to fit
the type specification.
:FORMATS defines which file format to pick
among possible matches in the data path."
  (check-type regexp list)
  (check-type function cl:function)
  (check-type formats (or boolean list cl:function))
  (when (or override (null (assoc name *type-registry*)))
    (push (list name regexp function formats) *type-registry*)))

;;
;;;; Asset Class
;;
;;

(defun get-types ()
  (remove-duplicates *dispatch-table*
                     :key #'dispatch-class
                     :from-end t))

(defclass asset ()
  ((formats
    :type (or null t list)
    :allocation 'class
    :documentation "List of possible file formats that can be used as asset source.")
   (namestring
    :type string
    :initarg :namestring
    :initform (error "Asset doesn't have a namestring.")
    :accessor asset-namestring
    :documentation "")
   (source
    :type pathname
    :initarg :source
    :accessor asset-source
    :documentation "")
   (exports
    :type list
    :documenatation "")
   (date
    :type string
    :documentation "")
   (description
    :type string
    :documentation "")
   (benchmark
    :type string
    :documentation "The data reported by TIME macro for measuring asset processor performance."))
  (:documentation ""))

(defgeneric export (asset)
  "")

(defmethod export ((asset asset)))

(defclass dispatcher ()
  ((class :initarg :class
          :type symbol
          :accessor dispatcher-class)
   (regex :initarg :regex
          :type list
          :accessor dispatcher-regex))
  (:documentation ""))

(defmethod initialize-instance :after ((dispatcher dispatcher))
  (push dispatcher *dispatch-table*))

(defgeneric dispatch (dispatcher namestring)
  (:documentation "Dispatch on an asset subclass."))

(defmethod dispatch ((dispatcher dispatcher) (namestring string))
  (some
   (lambda (regex)
     (scan (create-scanner regex :case-insensitive-mode t)
           namestring))
   (dispatcher-regex dispatcher)))
