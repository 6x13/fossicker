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
;;; Config
;;
;;

(declaim (type (or symbol string) *default-config-system*))

(defvar *default-config-system* '#:fossickerrc
  "Configuration system name to be used when no system name is supplied as an argument to the SYSTEM parameter in a call to CONFIGURE.")

(defclass configuration (asdf:package-inferred-system)
  ((projects :initarg :projects
             :initform nil
             :accessor projects)
   (default  :initarg :default
             :initform nil
             :accessor default-project)
   (legend   :initarg :legend
             :initform nil
             :accessor legend))
  (:documentation
   "System definition class for Fossicker configuration."))

(declaim (type (or null configuration) *config*))

(defvar *config* nil
  "System object that holds configuration data.")

(defvar *repository* nil
  "Directory containing the  Fossicker package.  It is used to  load extra data
  distributed with the source.  Its value  is computed from the location of the
  Fossicker system.")

(defun configure (&key (system *default-config-system*) reloadp)
  "Loads the configuration system"
  (check-type system (or symbol string))
  (funcall (if reloadp #'load-system #'require-system) system)
  (setf *config* (find-system system))
  (setf *repository* (system-source-directory '#:fossicker))
  (load-projects *config*)
  (set-project (default-project *config*)))

;;
;;;; Project Operations
;;
;;

(defgeneric load-projects (config)
  (:documentation
   "Loads all projects listed in PROJECT slot of CONFIG.")
  (:method ((config configuration))
    (clear-project-registry)
    (dolist (project (projects config))
      (assert (and project (listp project)))
      (load-project (getf project :file) (getf project :root)))))

(defgeneric add-project (config file &optional root)
  (:documentation
   "Add path and root to PROJECTS slot of CONFIG if not already added.")
  (:method ((config configuration) file &optional root)
    (pushnew (if root (list :file file :root root) (list :file file))
             (projects config)
             :key (lambda (project) (getf project :file))
             :test #'string=)))

(defgeneric delete-project (config file)
  (:documentation
   "Remove project from PROJECTS slot of CONFIG if exists.")
  (:method ((config configuration) file)
    (setf (projects config)
          (delete file
                  (projects config)
                  :key (lambda (project) (getf project :file))
                  :test #'string=))))
