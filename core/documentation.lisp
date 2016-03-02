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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation Extensions
;;
;;
;;;; Documentation
;;
;;

;; With  the following  method definitions  on generic  function DOCUMENTATION,
;; "documentation (x symbol)  (doc-type (eql 'type))" form  works flawlessly in
;; CCL, ECL and CLISP  while it doesn't work on SBCL and  CMUCL because they do
;; not implement documentation  methods that specialize on symbols  in terms of
;; their   class  counterparts.   Nothing  to   do  about   it  except   fixing
;; implementations.

(defmethod documentation ((x layered-class) (doc-type (eql 't)))
  (documentation
   (getf (partial-class-defining-classes x) 't)
   'type))

(defmethod documentation ((x layered-class) (doc-type (eql 'type)))
  (documentation x 't))

(defmethod (setf documentation) (new-value
                                 (x layered-class)
                                 (doc-type (eql 't)))
  (setf (documentation
         (getf (partial-class-defining-classes x) 't)
         't) new-value))

(defmethod (setf documentation) (new-value
                                 (x layered-class)
                                 (doc-type (eql 'type)))
  (setf (documentation x 't) new-value))


;;
;;;; Documentation*
;;
;;

;; A more complete documentation of  the thing, taking metaclass related issues
;; into account. Tailored with layered-class and similar scenarios in mind.

(defgeneric documentation* (x doc-type)
  (:documentation "A more complete documentation of the thing, taking metaclass
  related issues into account."))

(defmethod documentation* (x doc-type)
  (documentation x doc-type))

(defmethod documentation* ((x symbol) (doc-type (eql 'type))
                           &aux (class (find-class x nil)))
  (if class
      (documentation* class 't)
      (call-next-method)))

(defmethod documentation* ((x layered-class) (doc-type (eql 'type)))
  "Formats  a  string containing  documentation  compiled  from each  LAYERED
  definition of the same CLASS."
  (documentation* x 't))

(defmethod documentation* ((x layered-class) (doctype (eql 't))
                           &aux docs)
  "Formats  a  string containing  documentation  compiled  from each  LAYERED
  definition of the same CLASS."
  (format nil "~{~a~^~%~}"
          (loop for (layer defining-class)
                  on (partial-class-defining-classes x)
                by #'cddr
                do (push (format nil "~@[~:@(~a Layer: ~)~]~a"
                                 (if (eql layer t) nil layer)
                                 (documentation defining-class 'type)) docs)
                finally (return docs))))

(defgeneric (setf documentation*) (new-value x doc-type)
  (:documentation "Setter function for DOCUMENTATION*. Signals an ERROR."))

(defmethod (setf documentation*) ((new-value t) (x t) (doc-type t))
  (error "DOCUMENTATION* does not provide setters. Use DOCUMENTATION."))
