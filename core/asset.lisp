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
    :type list
    :initform nil
    :accessor asset-formats
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
  (:documentation "The default asset class."))

(defgeneric compute-prospectable-formats (asset)
  (:documentation   "Gets  prospectable   formats   and  adds   upcase/downcase
  versions.")
  (:method ((asset asset)) nil)
  (:method :around ((asset asset))
    "Compiles  a new  list  with  the upcase-downcase  variations  of the  list
returned by PRIMARY method."
    (apply #'append
           (mapcar (lambda (elt)
                     (list (string-downcase elt)
                           (string-upcase elt)))
                   (call-next-method)))))

(defmethod initialize-instance :after ((instance asset) &key)
  (setf (asset-formats instance) (compute-prospectable-formats instance)))

(defgeneric save (asset)
  (:documentation "Saves asset and creates the corresponding file[s] on project
  path. Default method  simply ignores the input and does  nothing.  Each asset
  subclass should implement their own export method.")
  (:method ((asset asset))))

(defclass prospect-any () (namestring)
  (:documentation    "Mixin     class    that     defines    a     method    of
  COMPUTE-PROSPECTABLE-FORMATS, which simply returns NIL. This is a dummy mixin
  to  make behaviour  explicit. The  default behaviour  of ASSET  class is  the
  same."))

(defmethod compute-prospectable-formats ((asset prospect-any))
  "Returns a list containing the extension of the file saved in NAMESTRING slot
of the ASSET."  nil)

(defclass prospect-same () (namestring)
  (:documentation    "Mixin     class    that     defines    a     method    of
  COMPUTE-PROSPECTABLE-FORMATS,   which  simply   returns   the  extension   of
  NAMESTRING slot value of ASSET instance."))

(defmethod compute-prospectable-formats ((asset prospect-same))
  "Returns a list containing the extension of the file saved in NAMESTRING slot
of the ASSET."
  (list (pathname-type (slot-value asset 'namestring))))

;;;;;;;;;;;;
;;; Prospect
;;
;;

(defun map-to-veins (namestring legend)
  "Recursively traverses LEGEND  matching all entries to  NAMESTRING. Returns a
list of each  matching entry bundled with their corresponding  positions in the
NAMESTRING to  be sorted later  accordingly.
Bundles  are in form  of (POSITION . VEINS))."
  (when legend
    (cons (cons (scan (caar legend) namestring)
                (cdar legend))
          (map-to-veins namestring (cdr legend)))))

(defun generate-vein-map (namestring class)
  "Gets  the vein  list using  MAP-TO-VEINS, sorting  veins according  to their
position  of occurrance.  Collects the  vein lists  appending them  into a  new
list. Conses the CLASS, which is used as root vein, to the generated list."
  (cons (string-downcase (symbol-name class))
        (apply #'append
               (mapcar #'cdr
                       (stable-sort 
                        (delete-if #'null
                                   (map-to-veins namestring
                                                 (legend *config*))
                                   :key #'car)
                        #'< :key #'car)))))

(defun prospect (map dir formats &optional prospect)
  "Traverses database and selects prospect."
  (if map
      (let ((new-dir (merge-pathnames*
                      (ensure-directory-pathname (car map))
                      (ensure-directory-pathname dir))))
        (prospect
         (cdr map)
         (or (directory-exists-p new-dir)
             (ensure-directory-pathname dir))
         formats
         (or (find-if (lambda (file)
                        (scan
                         (format nil "~a\\.(~{~a~^|~})" (car map) formats)
                         (file-namestring file)))
                      (directory-files dir))
             prospect)))
      prospect))

(defun compile-path (spec)
  (merge-pathnames* 
   (ensure-directory-pathname (or (car spec) ""))
   (merge-pathnames*
    (ensure-directory-pathname (project-path *project*))
    (ensure-directory-pathname (project-root *project*)))))

(defun report (result)
  (message (if (listp result)
               (format nil "~a assets generated!~%"
                       (if result (length result) "No"))
               "Finished!~%")))
