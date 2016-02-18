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
;;;; Dispatcher
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
    :documentation "List of prospectable file formats, meaning, extensions that
    can  be  used  as  asset  source.   NIL  means  no  restrictions  apply  to
    prospectable asset formats, any file will do.")
   (path
    :type (or null string)
    :initform nil
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
    :initform nil
    :accessor asset-files
    :documentation  "List of  FILE class  instances that  are generated  by the
    asset.")
   (date
    :type integer
    :initform (get-universal-time)
    :reader asset-date
    :documentation "Date of creation for ASSET instance.")
   (description
    :type string
    :documentation "Description of the asset to be used in generated report.")
   (benchmark
    :type string
    :documentation  "The  data  reported  by TIME  macro  for  measuring  asset
    processor performance."))
  (:documentation "The default asset class."))

(defmethod shared-initialize :after ((instance asset) slot-names &key)
  (setf (asset-formats instance) (restrict-prospectable-formats instance))
  (setf (asset-source instance) (prospect-asset instance)))

;;
;;;; Prospectable Format Restrictions
;;
;;

(defgeneric restrict-prospectable-formats (asset)
  (:documentation   "Gets  prospectable   formats   and  adds   upcase/downcase
  versions.")
  (:method ((asset t))
    "Sane default. No prospectable format restrictions."
    (warn "Asset subclass named ~A didn't specify prospectable format restriction behaviour. Assuming no restrictions."
          (type-of asset)) nil)
  (:method :around ((asset asset))
    "Compiles  a new  list  with  the upcase-downcase  variations  of the  list
returned by PRIMARY method."
    (apply #'append
           (mapcar (lambda (elt)
                     (list (string-downcase elt)
                           (string-upcase elt)))
                   (call-next-method)))))

(defclass prospect-any () (namestring)
  (:documentation    "Mixin     class    that     defines    a     method    of
  RESTRICT-PROSPECTABLE-FORMATS,  which simply  returns  NIL. This  is a  dummy
  mixin to make behaviour explicit. The default behaviour of ASSET class is the
  same."))

(defmethod restrict-prospectable-formats ((asset prospect-any))
  "Returns NIL,  meaning no restrictions  apply to source  format prospection."
  nil)

(defclass prospect-same () (namestring)
  (:documentation    "Mixin     class    that     defines    a     method    of
  RESTRICT-PROSPECTABLE-FORMATS,   which  simply   returns  the   extension  of
  NAMESTRING slot value of ASSET instance."))

(defmethod restrict-prospectable-formats ((asset prospect-same))
  "Returns a list containing the extension of the file saved in NAMESTRING slot
of the ASSET."
  (list (pathname-type (slot-value asset 'namestring))))

(defclass prospect-custom () (namestring)
  (:documentation    "Mixin     class    that     defines    a     method    of
  RESTRICT-PROSPECTABLE-FORMATS, which signals an error when called. This class
  is  mainly  for  stating  asset   behaviour  explicitly,  and  for  debugging
  purposes."))

(defmethod restrict-prospectable-formats ((asset prospect-custom))
  "Signals    an    error    because    some   other    primary    method    on
RESTRICT-PROSPECTABLE-FORMATS  that   precedes  this   one  should   have  been
supplied."
  (error  "Asset  subclass named ~A chose  to restrict  prospectable  formats  manually without actually specifying a restriction method."
          (type-of asset)))

;;
;;;; Asset Generics
;;
;;

;; TODO
(defgeneric save (asset)
  (:documentation "Saves asset and creates the corresponding file[s] on project
  path. Default method  simply ignores the input and does  nothing.  Each asset
  subclass should implement their own export method.")
  (:method ((asset asset))))

;; TODO
(defgeneric browse (asset)
  (:documentation  "Browse the file using the web browser.")
  (:method ((asset asset) &aux file) nil))

;;
;;;; Prospect
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

(defgeneric generate-vein-map (asset)
  (:documentation  "Gets  the  vein  list  using  MAP-TO-VEINS,  sorting  veins
according to their  position of occurrance.  Collects the  vein lists appending
them into a new  list.  Conses the class of ASSET, which is  used as root vein,
to the generated list.")
  (:method ((asset asset))
    (cons (string-downcase (type-of asset))
          (apply #'append
                 (mapcar #'cdr
                         (stable-sort 
                          (delete-if #'null
                                     (map-to-veins (asset-namestring asset)
                                                   (legend *config*))
                                     :key #'car)
                          #'< :key #'car))))))

(defun prospect (map dir formats &optional prospect
                 &aux (dir (ensure-directory-pathname dir))
                   (new-dir (subpathname dir (car map) :type :directory)))
  "Traverses mine and selects prospect."
  (assert dir nil "Directory can't be NIL.")
  (if map
      (prospect
       (cdr map)
       (or (directory-exists-p new-dir) dir)
       formats
       (or (find-if (lambda (file)
                      (scan
                       (format nil "~a\\.(~{~a~^|~})" (car map) formats)
                       (file-namestring file)))
                    (directory-files dir))
           prospect))
      prospect))

(defgeneric prospect-asset (asset)
  (:documentation  "Prospects asset in the MINE specified in *CONFIG*.")
  (:method ((asset asset))
    (prospect (generate-vein-map asset)
              (mine *config*)
              (asset-formats asset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-path (spec)
  (subpathname
   (subpathname (project-root *project*) (project-path *project*))
   (car spec) :type :directory))

(defun report (result)
  (message (if (listp result)
               (format nil "~a assets generated!~%"
                       (if result (length result) "No"))
               "Finished!~%")))
