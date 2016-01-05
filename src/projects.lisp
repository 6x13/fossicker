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

(defvar *project-registry* nil
  "List of fossicker project instances.")

(defvar *project* nil
  "Name of the currently selected fossicker project.")

(defclass project ()
  ((name
    :initarg :name
    :reader project-name
    :documentation "The name of the project. It has to be unique.")
   (file
    :initarg :file
    :reader project-file
    :documentation "Path to loaded project configuration file.")
   (root
    :initarg :root
    :reader project-root
    :documentation "Project root directory. It is either explicitly defined in project file or the directory project file resides in is used. ")
   (path
    :initarg :path
    :reader project-path
    :documentation "The path to the project resource directory.")
   (specs
    :initarg :specs
    :reader project-specs
    :documentation "Association list of asset type specifications.")
   (assets
    :initform nil
    :reader project-assets
    :documentation "The list of asset instances generated in current session. The CAR is the latest generated asset.")))

;;
;;;; Load Projects
;;
;;

(defun add-project (path &optional root)
  "Add path to projects if not already added."
  (pushnew (list path root)
           (getf *config* :projects)
           :key #'car
           :test #'string=))

(defun remove-project (path)
  "Remove path from projects if not already added."
  (delete path
          (getf *config* :projects)
          :key #'car
          :test #'string=))

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
      (push (make-instance 'project
                           :name (car data)
                           :file (car proj)
                           :root (or (cadr proj)
                                     (pathname-directory-pathname (car proj)))
                           :path (cadr data)
                           :specs (cddr data))
            *project-registry*))))

;;
;;;; Get Project
;;
;;

(defun get-project ()
  (find *project* *project-registry* :key #'project-name :test #'string=))

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
the current working directory path to select the project."
  (projects-assert)
  (setf *project* (project-name (find-project *project-registry*)))
  (show-current-project))
