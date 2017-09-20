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

(deftype pathname-designator ()
  "Pathname designator type definition specifying types allowed in Hyperspec to
  be used in most pathname related functions."
  '(or pathname string))

(define-layered-class project ()
  ((file
    :initarg :file
    :type pathname-designator
    :initform (error "Project doesn't have an associated configuration file.")
    :reader project-file
    :documentation "Path to loaded project configuration file.")
   (name
    :initarg :name
    :type string
    :initform (error "Name should have been inferred during initialization.")
    :reader project-name
    :documentation "The name of the project. It has to be unique.")
   (root
    :initarg :root
    :type pathname-designator
    :initform (error "Root should have been inferred during initialization.")
    :reader project-root
    :documentation "Project root directory. It  is either explicitly defined in
    project file or the directory project file resides in is used. It should be
    an absolute pathname.")
   (path
    :initarg :path
    :type pathname-designator
    :initform nil
    :reader project-path
    :documentation "Path of  project resource directory. It can  be an absolute
    path  or a  relative path.  If  relative, it  is  merged with  the ROOT  to
    retrieve the absolute path of resource directory.")
   (specs
    :initarg :specs
    :type list
    :initform nil
    :reader project-specs
    :documentation "Association list of asset type specifications.")
   (legend
    :initarg :legend
    :type list
    :initform nil
    :reader project-legend
    :documentation "Project specific legend.")
   (mine
    :initarg :mine
    :type (or null pathname-designator)
    :initform nil
    :accessor project-mine
    :documentation  "Project specific  pathname  of the  mine  to prospect  for
    sources.")
   (draft
    :type (or null function asset)
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
    :documentation "Asset that is in edit mode.  Its RQSTSTRING slot is static.
    Therefore it cannot change-class. All other  properties of the asset can be
    edited. Selected asset may or may not already be in database. When draft is
    submitted, it is set as selected but it is not saved in database yet.")
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
                 ;; elsewhere. We specifically infer them  here in order to let
                 ;; any possible user specified initarg overrides them.
                 (list :name (infer-project-name file)
                       :root (infer-project-root file)))))

;;
;;;; Asset Operations
;;
;;

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
       (ctypecase (project-draft project)
         (null
          (message "No draft asset available to select for project named ~a."
                   (project-name project)))
         (function
          (message "Draft not yet suitable to select for project named ~a."
                   (project-name project)))
         (asset
          (message "Setting draft as selected for project named ~a."
                   (project-name project))
          (setf (project-selected project) (project-draft project))
          (setf (project-draft project) nil)))))
    (project-selected project)))

(defun matching-spec (dispatch specs)
  "Iterates over project  specs and returns first asset  spec that successfully
dispatched  on RQSTSTRING.   Project specification  order drives  type dispatch
precedence, not the dispatch list."
  (when specs
    (if (member (caar specs) dispatch)
        (car specs)
        (matching-spec dispatch (cdr specs)))))

(defun infer-asset-initargs (rqststring project initargs)
  "Adds  defaulted  :LEGEND and  :MINE  and  :RQSTSTRING keywords  to  INITARGS
specified in project spec of dispatched asset class."
  (apply #'list
         :rqststring rqststring
         :legend (or (project-legend project)
                     (legend *config*))
         :mine (or (project-mine project)
                   (mine *config*))
         initargs))

(defun initialize-draft (draft class initargs)
  "Decides on how  to initialize new draft. If necessary,  makes an instance of
asset subclass,  set it as DRAFT.  Otherwise, reuses DRAFT. This  is refactored
into a  seperate function in  order to safely  optimize draft generation  in an
isolated manner."
  (cond
    ((and draft (eq (type-of draft) class))
     ;; If DRAFT exists and the CLASS matches, just reinitialize with INITARGS.
     (apply #'reinitialize-instance draft initargs))
    ;; DISABLED! LOCAL-SLOTS that  accidentally have the same name  will not be
    ;; reinitialized with INITFORM of changed CLASS.
    ;; (draft
    ;;  ;; If DRAFT exists and CLASS is different,  change class with INITARGS.
    ;;  (apply #'change-class draft class initargs))
    (t
     ;; Otherwise, make an instance of CLASS and return it.
     (apply #'make-instance class initargs))))

(defun compile-draft-closure (draft class initargs)
  "If the  DRAFT is  a CLOSURE,  calls CLOSURE  with no  arguments in  order to
retrieve the ASSET it holds without further processing it, for use within newly
compiled  CLOSURE ,  then  calls  itself using  the  returned  ASSET as  DRAFT.
Otherwise, just  compiles a  CLOSURE and returns  the compiled  CLOSURE. Design
would be somehow simpler if the CLOSURE and ASSET were stored in seperate slots
of the  PROJECT. But that would  be misleading, giving the  impression they can
both  be   incorporated  into  pipeline  independently.    The  current  design
emphasizes the fact that there should either be a CLOSURE expected to be called
in  order to  generate an  ASSET, or  an ASSET  that is  expected to  either be
DISCARDED or SELECTED. In a way, the value  of the DRAFT slot of a PROJECT is a
direct representation of the current state of a drafting process."
  (if (functionp draft)
      (compile-draft-closure (funcall draft) class initargs)
      (lambda (&optional (interactive nil interactive-supplied-p))
        (if interactive-supplied-p
            (initialize-draft draft class (append interactive initargs))
            draft))))

(defgeneric draft (project rqststring)
  (:documentation "Generates a draft CLOSURE for PROJECT using RQSTSTRING.")
  (:method ((project project) rqststring
            &aux (dispatch (dispatch rqststring))
              (spec (matching-spec dispatch (project-specs project)))
              (class (car spec))
              (initargs (infer-asset-initargs rqststring
                                              project
                                              (cdr spec))))
    (assert (stringp rqststring) nil "~a is not a filename." rqststring)
    (cond (spec
           ;; Compile and store DRAFT CLOSURE.
           (setf (project-draft project)
                 (compile-draft-closure (project-draft project)
                                        class
                                        initargs))
           (message "Asset draft prepared for class ~A." class)
           class)
          (dispatch
           ;; No matching specification in project. Do nothing.
           (message "~A Candidates: ~A"
                    "No dispatched asset class is specified in project."
                    dispatch)
           nil)
          (t
           ;; Couldn't dispatch on any classes.
           (message "Couldn't dispatch on any asset class.")
           nil))))

(defgeneric submit (project interactive)
  (:documentation "Completes draft ASSET for PROJECT using INTERACTIVE.")
  (:method ((project project) interactive)
    (check-type (project-draft project) function)
    (message "Submitting the draft for project named ~a."
             (project-name project))
    (setf (project-draft project)
          (funcall (project-draft project) interactive))
    (select project)))

;; TODO
(defgeneric redraft (project &key clean)
  (:documentation "Removes  selection from  ASSETS slot and  moves it  to DRAFT
  slot.  Then  sets  SELECTED to  NIL.  If  CLEAN  is  T, REDRAFT  will  remove
  previously generated and discarded files from file system.")
  (:method ((project project) &key (clean t))))

(defgeneric generate (project)
  (:documentation "TEMP: Ensures SELECTED is in ASSETS.")
  (:method ((project project) &aux (result nil))
    (if (project-selected project)
        (progn (pushnew (project-selected project)
                        (project-assets project))
               ;; TODO
               (message (if (listp result)
                            (format nil "~a assets generated!"
                                    (if result (length result) "No"))
                            "Finished!")))
        (message "Skipping generation. No asset selected for project named ~a."
                 (project-name project)))))

;;
;;;; Project Operations
;;
;;

(defun get-history-path (project
                         &aux (file (project-file project)))
  (merge-pathnames* (format nil "~a.history.~a"
                            (directory-namestring file)
                            (file-namestring file))))

(defun save-project-history (&optional name
                             &aux (project (if name
                                               (get-project name)
                                               *project*)))
  "Save named project history, or if null, save active project history."
  (assert project nil "No suitable projects to process.")
  (with-open-file (out (get-history-path project)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (loop for asset in (project-assets project)
          doing (format out "~&(~{~1,0T~12S ~S~^~%~})" (marshal asset))))
  (message "History for the project ~a saved to disk." (project-name project)))

(defun load-project-history (&optional name
                             &aux (project (if name
                                               (get-project name)
                                               *project*))
                               (path (get-history-path project)))
  "Load named project history, or if null, load active project history."
  (assert project nil "No suitable projects to process.")
  (if (probe-file path)
      (with-open-file (in path :direction :input)
        (loop with eof = (gensym)
              for asset = (allocate-instance (find-class 'asset))
              for data = (read in nil eof)
              until (eq data eof)
              collecting (unmarshal asset data) into assets
              finally (setf (project-assets project) assets))
        (message "History for the project ~a loaded from disk." (project-name project)))
      (message "No history for the project ~a found on disk." (project-name project))))

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
  "If  NAME is  supplied, manually  select a  project by  name among  Fossicker
projects  list.  Otherwise  automatically  select  a  project  among  Fossicker
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
  (message "Fossicker Project currently set to ~a."
           (if *project*
               (project-name *project*)
               "nothing")))

(defun clear-project-registry ()
  "Sets the *PROJECT-REGISTRY* to NIL."
  (setf *project-registry* nil))

(defun load-project (&rest initargs &key file (import t) &allow-other-keys)
  "Loads the project specified in FILE into *PROJECT-REGISTRY*."
  (let* ((project (apply #'make-instance
                         (infer-project-class file)
                         :import import
                         initargs))
         (name (project-name project)))
    (push project *project-registry*)
    (load-project-history name)))

(defun unload-project (name)
  "Unloads the project with the NAME from *PROJECT-REGISTRY*."
  (setf *project-registry* (delete (get-project name) *project-registry*)))
