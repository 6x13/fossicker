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

(declaim (type (or null project) *project*))

(defvar *project* nil
  "Name of the currently selected fossicker project.")

(defclass project ()
  ((name
    :initarg :name
    :initform (error "Project doesn't have a name.")
    :type string
    :reader project-name
    :documentation "The name of the project. It has to be unique.")
   (file
    :initarg :file
    :initform (error "Project doesn't have a file.")
    :type pathname
    :reader project-file
    :documentation "Path to loaded project configuration file.")
   (root
    :initarg :root
    :type pathname
    :reader project-root
    :documentation "Project root directory. It is either explicitly defined in project file or the directory project file resides in is used. ")
   (path
    :initarg :path
    :initform nil
    :type pathname
    :reader project-path
    :documentation "The path to the project resource directory.")
   (specs
    :initarg :specs
    :initform nil
    :type list
    :reader project-specs
    :documentation "Association list of asset type specifications.")
   (current
    :type asset
    :initform nil
    :accessor current-asset
    :documentation "Asset that is currently edited. Same as the CAR of generated assets.")
   (assets
    :type list
    :initform nil
    :accessor project-assets
    :documentation "The list of asset instances generated in current session. The CAR is the latest GENERATED asset.")
   (log
    :type list
    :initform nil
    :reader project-log
    :documentation "Reports of project related actions.")))

(defun get-data-from-file (path)
  "Read s-expression from PATH."
  (assert (file-exists-p path) nil "File ~a doesn't exist." path)
  (with-open-file (in path :external-format :utf-8)
    (read in)))

(defmethod initialize-instance :before ((project project) &key file importp)
  (when importp
    (let ((data (get-data-from-file file)))
      (with-slots (name path specs) project
        (setf name (car data)
              path (cadr data)
              specs (cddr data))))))

(defmethod initialize-instance :after ((project project) &key)
  (with-slots (file) project
    (unless (slot-boundp project 'root)
      (setf (slot-value project 'root) (pathname-directory-pathname file)))))

(defgeneric generate (project)
  (:documentation
   "Generates asset, pushes it to ASSETS and sets it as CURRENT.")
  (:method ((project project))))

;;
;;;; Selection
;;
;;

(defun project-root-p (project-root)
  (when *default-pathname-defaults*
    (let ((project (namestring (truename project-root)))
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

(defun get-project (name)
  (find name *project-registry* :key #'project-name :test #'string=))

(defun set-project (&optional name)
  "If  NAME is  supplied, manually  select a  project by  name among  fossicker
projects  list.  Otherwise  automatically  select  a  project  among  fossicker
projects list.  Checks the project root  of each fossicker project  against the
current working directory path to select the project."
  (assert *project-registry* nil
          "No fossicker projects defined. You need at least one.")
  (assert (or (null name)
              (member name
                      *project-registry*
                      :key #'project-name
                      :test #'string=))
          nil "~a is not in project list." name)
  (setf *project* (if name
                      (get-project name)
                      (find-project *project-registry*)))
  (message "Fossicker Project currently set to ~a.~%"
           (if *project*
               (project-name *project*)
               "nothing")))

(defun clear-project-registry ()
  (setf *project-registry* nil))

(defun load-project (file &optional root)
  "Loads the project specified in FILE into *PROJECT-REGISTRY*."
  (push (apply #'make-instance
               'project
               :importp t
               :file file
               (if root (list :root root)))
        *project-registry*))

(defun unload-project (name)
  "Unloads the project with the NAME from *PROJECT-REGISTRY*."
  (setf *project-registry* (delete (get-project name) *project-registry*)))

