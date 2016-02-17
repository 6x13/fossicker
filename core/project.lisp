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
    project file or the directory project file resides in is used.")
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
   (draft
    :type (or null asset)
    :initform nil
    :accessor project-draft
    :documentation "Asset that  is currently being sketched. Draft  is an asset
    in volatile  state. It can  be safely discarded.  A  draft can only  be the
    result of new draft op. It is an  asset that is guaranteed to be unsaved so
    far  so  there  are  no  side-effects  on  file-system  that  needs  to  be
    processed (e.g cleaned) before modifying it. A draft is isolated from asset
    database to safely prototype the effects  of drafted asset. In other words,
    it exists for interactive dry-run kind of actions.")
   (selected
    :type (or null asset)
    :initform nil
    :accessor project-selected
    :documentation "Asset that is in edit mode.  Its NAMESTRING slot is static.
    Therefore it cannot change-class. All other  properties of the asset can be
    edited. Selected asset may or may not be already in database. When draft is
    accepted, it is set as selected but it is not saved in database yet.")
   (assets
    :type list
    :initform nil
    :accessor project-assets
    :documentation  "The   list  of   asset  instances  generated   in  current
    session. The CAR is the latest saved asset.")
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
  "Read one S-EXPRESSION from PATH."
  (assert (file-exists-p path) nil "File ~a doesn't exist." path)
  (with-open-file (in path :external-format :utf-8)
    (let ((*package* (find-package :fossicker))) (read in))))

(defun infer-project-root (pathname)
  "Infers  the  root  of  project  that is  generated  from  the  project  file
location."
  (pathname-directory-pathname pathname))

(defun infer-project-name (pathname)
  "Infers the  name of project to  generate from project file  name. If project
file is hidden using  DOT at the beginning of filename, the  dot is removed for
convenience."
  (remove #\. (pathname-name pathname) :end 1))

(defun infer-project-class (pathname)
  "Infers the class of project to generate from project file extension. PROJECT
class is used if there is not extension."
  (or (intern (string-upcase (pathname-type pathname))
                           :fossicker)
                   'project))

(defmethod initialize-instance :around
    ((instance project)
     &rest initargs
     &key import file)
  "Reads  the data  from  PROJECT FILE  and calls  next  method with  arguments
appending  the  inferred  values,  initialization arguments  and  project  file
data. Binds the ROOT and NAME of project by inferring them from project file if
not explicitly stated."
  (declare (type (or pathname string) file))
  (apply #'call-next-method instance
         (append initargs
                 ;; When IMPORT is T, append data from PROJECT FILE.
                 (when import
                   (assert (file-exists-p file) nil
                           "Project file doesn't exist.")
                   (get-data-from-file file))
                 ;; Infer  NAME  and  ROOT,  in case  they  are  not  specified
                 ;; elsewhere.
                 (list :name (infer-project-name file)
                       :root (infer-project-root file)))))

(defgeneric draft (project namestring)
  (:documentation "Generates draft asset for PROJECT using NAMESTRING.")
  (:method ((project project) namestring)
    (labels ((matching-spec (types specs)
               "Iterates over project  specs and returns first  asset spec that
successfully dispatched on namestring.  Project specification order drives type
dispatch precedence, not the dispatch list."
                (when specs
                  (if (member (caar specs) types)
                      (car specs)
                      (matching-spec types (cdr specs)))))
             (initialize-draft (project class namestring initargs)
               "Decides on how to initialize new draft."
               (let ((draft (project-draft project)))               
                 (if draft
                     (if (eq (type-of draft) class)
                         (apply #'reinitialize-instance draft
                                :namestring namestring
                                initargs)
                         (apply #'change-class draft class
                                :namestring namestring
                                initargs))
                     (setf (project-draft project)
                           (apply #'make-instance class
                                  :namestring namestring
                                  initargs))))))
      (assert (stringp namestring) nil "~a is not a filename." namestring)
      (let* ((dispatch (dispatch namestring))
             (spec (matching-spec dispatch (project-specs project)))
             (class (car spec))
             (initargs (cdr spec)))
        (cond (spec
               ;; If necessary, make  an instance of asset subclass,  set it as
               ;; DRAFT. Otherwise, reuse DRAFT.
               (initialize-draft project class namestring initargs)
               (message "Asset generated: ~A" (project-draft project)))
              (dispatch
               ;; No matching specification in project. Do nothing.
               (message "~A Candidates: ~A"
                        "No dispatched asset class is specified in project."
                        dispatch))
              (t
               ;; Couldn't dispatch on any classes.
               (message "Couldn't dispatch on any asset class.")))))))

(defgeneric select (project &optional index)
  (:documentation "If INDEX is supplied, sets SELECTED to the ASSET instance at
  the INDEX position of ASSETS slot,  otherwise sets DRAFT as SELECTED and sets
  DRAFT to NIL.")
  (:method ((project project) &optional index)
    (ctypecase index
      (unsigned-byte
       (let ((instance (nth index (project-assets project))))
         (if instance
             (progn
               (message "Changed selection for project named ~a."
                        (project-name project))
               (setf (project-selected project) instance))
             (message 
              "No asset at position ~a for project named ~a."
              index (project-name project)))))
      (null
       (if (project-draft project)
           (progn
             (message "Setting draft as selected for project named ~a."
                       (project-name project))
             (setf (project-selected project) (project-draft project))
             (setf (project-draft project) nil))
           (message "No draft asset available to select for project named ~a."
                     (project-name project)))))))

(defgeneric generate (project)
  (:documentation "TEMP: Ensures SELECTED is in ASSETS.")
  (:method ((project project))
    (if (project-selected project)
        (pushnew (project-selected project)
                 (project-assets project))
        (message "Skipping generation. No asset selected for project named ~a."
                 (project-name project)))))

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
  "Sets the *PROJECT-REGISTRY* to NIL."
  (setf *project-registry* nil))

(defun load-project (&rest initargs &key file (import t) &allow-other-keys)
  "Loads the project specified in FILE into *PROJECT-REGISTRY*."
  (push (apply #'make-instance
               (infer-project-class file)
               :import import
               initargs)
        *project-registry*))

(defun unload-project (name)
  "Unloads the project with the NAME from *PROJECT-REGISTRY*."
  (setf *project-registry* (delete (get-project name) *project-registry*)))

