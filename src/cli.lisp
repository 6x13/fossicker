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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command Line Interface
;;
;;

(defun message (&rest args)
  (apply #'format (cons t args)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-source (prospect)
  (pathname
   (if (and prospect (y-or-n-p "Source: ~a" prospect))
       prospect
       (prompt-read "Source"))))

(defun prompt-context (filename)
  (pathname-as-directory
   (let ((context (pathname-directory-pathname filename)))
     (if (y-or-n-p "Context: ~a" context)
         context
         (pathname (prompt-read "Context"))))))

(defun generate (filename)
  "Generates the asset according to the double-quoted text
at current cursor position."
  (assert (stringp filename) nil "~a is not a filename." filename)
  (assert *project* nil "No fossicker project selected for current buffer.")
  (let* ((fname (pathname filename))
         (ext (pathname-type fname))
         (types (matching-types (namestring fname)))
         (specs (cdddr (get-project)))
         (type (matching-spec types (mapcar #'car specs)))
         (spec (cdr (assoc type specs)))
         (fn (elt (assoc type *type-registry*) 2))
         (formats (get-extension-list type ext))
         (context (prompt-context fname))
         (path (compile-path spec))
         source)
    (assert types nil
            "Couldn't match file name ~a to regexp list of any fossicker type."
            fname)
    (assert type nil "No matching type is included in project. Possible types: ~a" types)
    (assert (listp formats) nil "Source dispatch function didn't return a list.")
    (setf source (prompt-source
                  (prospect
                   (generate-vein-map (namestring fname) type)
                   (if (getf *config* :data-path)
                       (pathname-as-directory (getf *config* :data-path))
                       (merge-pathnames-as-directory
                        fossicker-conf:*basedir*
                        "data/"))
                   (add-case-variations formats))))
    (assert (file-exists-p source) nil
            "Source ~a is not a regular file." source)
    (assert
     (or (null formats) (scan (format nil "\\.(~{~a~^|~})" formats)
                              (file-namestring source)))
     nil "Source expected to be one of following formats: ~a. Got ~a."
     formats (pathname-type source))
    (report
     (funcall fn path context
              (file-namestring fname)
              ext (cdr spec) source))))
