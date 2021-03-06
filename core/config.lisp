;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 23 October 2015

;; This file is part of Fossicker.

;; Fossicker is free  software: you can redistribute it and/or  modify it under
;; the  terms of  the  GNU General  Public  License as  published  by the  Free
;; Software Foundation,  either version 3 of  the License, or (at  your option)
;; any later version.

;; Fossicker is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without  even the implied  warranty of MERCHANTABILITY  or FITNESS
;; FOR  A PARTICULAR  PURPOSE.  See  the GNU  General Public  License for  more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; Fossicker.  If not, see <http://www.gnu.org/licenses/>.

(in-package :fossicker)

;;;;;;;;;;
;;; Config
;;
;;

(declaim (type (or symbol string) *default-config-system*))

(defvar *default-config-system* '#:fossickerrc
  "Configuration system name to  be used when no system name  is supplied as an
  argument to the SYSTEM parameter in a call to CONFIGURE.")

(declaim (type (or null configuration) *config*))

(defvar *config* nil
  "System object that holds configuration data.")

(defvar *repository* nil
  "Directory containing the  Fossicker package.  It is used to  load extra data
  distributed with the source.  Its value  is computed from the location of the
  Fossicker system.")

(defvar *default-mine-directory* (ensure-directory-pathname "mine")
  "The relative directory path default mine resides in.")

(defclass configuration (package-inferred-system)
  ((notice
    :initarg :notice
    :type boolean
    :initform t
    :reader startup-notice
    :documentation "Enable/disable Fossicker startup notice.")
   (projects
    :initarg :projects
    :type list
    :initform nil
    :accessor projects
    :documentation  "Fossicker project  file  locations. Project  ROOT will  be
    computed from project file location if not explicitly specified.")
   (default
    :initarg :default
    :type (or null string)
    :initform nil
    :accessor default-project
    :documentation  "Default  project name  to  be  selected. Project  will  be
    autoselected from current working directory if not specified.  Project will
    be set to NIL  if there is no project root that  corresponds to the current
    working directory.")
   (legend
    :initarg :legend
    :type list
    :initform nil
    :accessor legend
    :documentation "Legend  to be used  when mapping a RQSTSTRING  specified by
    user to a prospect template from database.")
   (mine
    :initarg :mine
    :type pathname-designator
    :initform (merge-pathnames* *default-mine-directory* *repository*)
    :accessor mine
    :documentation "Pathname of the mine to prospect for sources."))
  (:documentation "System definition class for Fossicker configuration."))

(defun configure (&key (system *default-config-system*) force)
  "Loads the configuration system."
  (check-type system (or symbol string))
  (clear-system system)
  ;; Set the repository before to initialize MINE slot of *CONFIG*.
  (setf *repository* (system-source-directory '#:fossicker))
  (if force
      (load-system system :force force)
      (require-system system))
  (setf *config* (find-system system))
  (load-projects *config*)
  (set-project (default-project *config*)))

;;
;;;; Project Operations
;;
;;

(defgeneric load-projects (config)
  (:documentation "Loads all projects listed in PROJECT slot of CONFIG.")
  (:method ((config configuration))
    (clear-project-registry)
    (dolist (project (projects config)) (apply #'load-project project))))

(defgeneric add-project (config file &optional root)
  (:documentation "Add FILE and ROOT to PROJECTS slot of CONFIG if absent.")
  (:method ((config configuration) file &optional root)
    (pushnew (if root (list :file file :root root) (list :file file))
             (projects config)
             :key (lambda (project) (getf project :file))
             :test #'string=)))

(defgeneric remove-project (config file)
  (:documentation "Remove project from PROJECTS slot of CONFIG if exists.")
  (:method ((config configuration) file)
    (setf (projects config)
          (delete file
                  (projects config)
                  :key (lambda (project) (getf project :file))
                  :test #'string=))))
