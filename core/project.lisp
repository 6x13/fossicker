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

;; (declaim (optimize                                                       
;;           (speed 0) (compilation-speed 0) (safety 3) (debug 3)))

;;;;;;;;;;;;
;;; Projects
;;
;;

(defvar *project-registry* nil
  "List of fossicker project instances.")

(declaim (type (or null project) *project*))

(defvar *project* nil
  "Name of the currently selected fossicker project.")

(define-layered-class project ()
  ((file
    :initarg :file
    :initform (error "Project doesn't have an associated file.")
    :type (or pathname string)
    :reader project-file
    :documentation "Path to loaded project configuration file.")
   (name
    :initarg :name
    :initform (error "Project doesn't have a name.")
    :type string
    :reader project-name
    :documentation "The name of the project. It has to be unique.")
   (root
    :initarg :root
    :type (or pathname string)
    :reader project-root
    :documentation "Project root directory. It  is either explicitly defined in
    project file or the directory project file resides in is used. ")
   (path
    :initarg :path
    :initform nil
    :type (or pathname string)
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
    :documentation  "Asset  that  is  currently  edited. Same  as  the  CAR  of
    generated assets.")
   (assets
    :type list
    :initform nil
    :accessor project-assets
    :documentation  "The   list  of   asset  instances  generated   in  current
    session. The CAR is the latest GENERATED asset.")
   (log
    :type list
    :initform nil
    :reader project-log
    :documentation "Reports of project related actions."))
  (:default-initargs
   :file (error "Project needs to have an associated file."))
  (:documentation "Base project class that can  be used and subclassed by other
  project types."))

(defun get-data-from-file (path)
  "Read s-expression from PATH."
  (assert (file-exists-p path) nil "File ~a doesn't exist." path)
  (with-open-file (in path :external-format :utf-8)
    (let ((*package* (find-package :fossicker))) (read in))))

(defun project-file-directory (project)
  (pathname-directory-pathname (project-file project)))

(defmethod initialize-instance :around
    ((instance project)
     &rest initargs
     &key import file (name (remove #\. (pathname-name file) :end 1)))
  (declare (type (or pathname string) file))
  (if import
      (let ((data (get-data-from-file file)))
        (apply #'call-next-method instance :name name (append initargs data)))
      (apply #'call-next-method instance :name name initargs)))

(defmethod initialize-instance :after ((instance project) &key)
  (unless (slot-boundp instance 'root)
    (setf (slot-value instance 'root)
          (project-file-directory instance))))

(defgeneric generate (project namestring)
  (:documentation "Generates asset, pushes it to ASSETS and sets CURRENT.")
  (:method ((project project) namestring)
    (labels ((matching-spec (types specs)
               "Iterates over project  specs and returns first  asset spec that
successfully dispatched on namestring.  Project specification order drives type
dispatch precedence, not the dispatch list."
                (when specs
                  (if (member (caar specs) types)
                      (car specs)
                      (matching-spec types (cdr specs))))))
      (let* ((dispatch (dispatch namestring))
             (spec (matching-spec dispatch (project-specs project))))
        (cond (spec
               ;; Make an  instance of  asset subclass, set  it as  CURRENT and
               ;; push it to ASSETS.
               (progn
                 (setf (current-asset project)
                       (apply #'make-instance (car spec)
                              :namestring namestring
                              (cdr spec)))
                 (push (current-asset project) (project-assets project))
                 (message "Asset generated: ~A"
                          (current-asset project))))
              (dispatch
               ;; No matching specification in project. Do nothing.
               (message "~A Candidates: ~A"
                        "No dispatched asset class is specified in project."
                        dispatch))
              (t
               ;; Couldn't dispatch on any classes.
               (message "Couldn't dispatch on any asset class.")))))))

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

(defun load-project (&rest initargs &key file (import t) &allow-other-keys)
  "Loads the project specified in FILE into *PROJECT-REGISTRY*."
  (push (apply #'make-instance
               (or (intern (string-upcase (pathname-type file))
                           :fossicker)
                   'project)
               :import import
               initargs)
        *project-registry*))

(defun unload-project (name)
  "Unloads the project with the NAME from *PROJECT-REGISTRY*."
  (setf *project-registry* (delete (get-project name) *project-registry*)))

