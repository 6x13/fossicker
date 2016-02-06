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

;;;;;;;;;;;;
;;; Prospect
;;
;;



(defun matching-spec (types specs)
  (when specs
    (if (member (car specs) types)
        (car specs)
        (matching-spec types (cdr specs)))))

(defun map-to-vein (string legend)
  (when legend
    (cons (cons (scan (caar legend) string)
                (cdar legend))
          (map-to-vein string (cdr legend)))))

(defun generate-vein-map (fname atype)
  (cons (string-downcase (symbol-name atype))
        (apply #'append
               (mapcar #'cdr
                       (stable-sort 
                        (delete-if #'null
                                   (map-to-vein fname (copy-alist (getf *config* :legend)))
                                   :key #'car)
                        #'< :key #'car)))))

(defun prospect (map dir formats &optional prospect)
  (if map
      (let ((ndir (merge-pathnames-as-directory
                   (pathname-as-directory dir)
                   (pathname-as-directory (car map)))))
        (prospect
         (cdr map)
         (or (directory-exists-p ndir)
             (pathname-as-directory dir))
         formats
         (or (find-if (lambda (file)
                        (scan
                         (format nil "~a\\.(~{~a~^|~})" (car map) formats)
                         (file-namestring file)))
                      (list-directory dir))
             prospect)))
      prospect))

(defun add-case-variations (formats)
  (apply #'append
         (mapcar (lambda (elt)
                   (list (string-downcase elt)
                         (string-upcase elt)))
                 formats)))

(defun get-extension-list (type ext)
  (let* ((formats (type-formats (assoc type *type-registry*))))
    (if formats
        (if (functionp formats)
            (funcall formats ext)
            (list ext))
        nil)))

(defun compile-path (spec)
  (canonical-pathname
   (merge-pathnames-as-directory
    (pathname-as-directory (project-root *project*))
    (pathname-as-directory (project-path *project*))
    (pathname-as-directory (or (car spec) "")))))

(defun report (result)
  (message (if (listp result)
               (format nil "~a assets generated!~%"
                       (if result (length result) "No"))
               "Finished!~%")))
