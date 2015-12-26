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
;;; Projects
;;
;;
;;;; Load Projects
;;
;;

(defvar *project-registry* nil
  "The list of fossicker project definitions.")

(defvar *project* nil
  "Name of the fossicker project buffer belongs to.")

(defun add-project (path)
  "Add path to projects if not already added."
  (pushnew path (getf *config* :projects) :test #'string=))

(defun remove-project (path)
  "Remove path from projects if not already added."
  (delete path (getf *config* :projects) :test #'string=))

(defun get-data-from-file (path)
  "Read s-expression from PATH."
  (assert (file-exists-p path) nil "File ~a doesn't exist." path)
  (with-open-file (in path :external-format :utf-8)
    (read in)))

(defun load-projects ()
  "Loads all projects in PROJECTS."
  (setf *project-registry* nil)
  (dolist (proj (getf *config* :projects))
    (let ((data (get-data-from-file (car proj))))
      (push (cons
             (car data)
             (cons
              (or (cadr proj)
                  (pathname-directory-pathname (car proj)))
              (cdr data)))
            *project-registry*))))

;;
;;;; Project Accessors
;;
;;

(defun get-project ()
  (assoc *project* *project-registry* :test #'string=))

(defun project-name (project)
  (elt project 0))

(defun project-root (project)
  (elt project 1))

(defun project-path (project)
  (elt project 2))

(defun project-specs (project)
  (cdddr project))

;;
;;;; Current Project
;;
;;

(defun show-current-project ()
  "Shows the current fossicker project in minibuffer."
  (message "Fossicker Project currently set to ~a.~%" (or *project* "nothing")))

;;
;;;; Selection
;;
;;

(defun projects-assert ()
  (assert *project-registry* nil
          "No fossicker projects defined. You need at least one."))

(defun set-project (project)
  "Manually select a project among fossicker projects list."
  (projects-assert)
  (assert (member project
                  *project-registry*
                  :key #'project-name
                  :test #'string=)
          nil "~a is not in project list." project)
  (setf *project* project)
  (show-current-project))

(defun unset-project ()
  "Set project to nil."
  (setf *project* nil)
  (show-current-project))

;;
;;;; Auto-Selection
;;
;;

(defun project-root-p (project-root)
  (when *default-pathname-defaults*
    (let ((project
            (namestring
             (truename (pathname-as-file project-root))))
          (current
            (namestring (truename *default-pathname-defaults*))))
      (scan (create-scanner (format nil "^~a" project)
                            :case-insensitive-mode t) current))))

(defun find-project (projects)
  (when projects
    (let ((proj (car projects)))
      (if (project-root-p (project-root proj))
          proj
          (find-project (cdr projects))))))

(defun auto-select-project ()
  "Automatically select a project among fossicker projects list.
Checks the project root of each fossicker project against
the current working directory path to find the project buffer
belongs to."
  (projects-assert)
  (setf *project* (car (find-project *project-registry*)))
  (show-current-project))
