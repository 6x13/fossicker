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
  (ensure-directory-pathname
   (let ((context (pathname-directory-pathname filename)))
     (if (y-or-n-p "Context: ~a" context)
         context
         (pathname (prompt-read "Context"))))))

;; (defun generate (filename)
;;   (assert *project* nil "No fossicker project selected for current buffer.")
;;   (let* ((fname (pathname filename))
;;          (context (prompt-context fname))
;;          (path (compile-path (matching-spec (dispatch fname) (project-specs *project*))))
;; 		 (source (prompt-source
;;                   (prospect-asset
;; 				   fname
;;                    (mine *config*)
;;                    (legend *config*)))))
;;     (assert (file-exists-p source) nil
;;             "Source ~a is not a regular file." source)
;;     ;; (assert
;;     ;;  (or (null formats) (scan (format nil "\\.(~{~a~^|~})" formats)
;;     ;;                           (file-namestring source)))
;;     ;;  nil "Source expected to be one of following formats: ~a. Got ~a."
;;     ;;  formats (pathname-type source))
;;     (report "ds")))
