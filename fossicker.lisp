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

;;; Fossicker Message

(defun message (&rest args)
  (apply #'format (cons t args)))


;;; Fossicker Config

(defvar *config-path* nil
  "Fossicker configuration file path.")

(defvar *config* nil
  "Fossicker configuration plist.")

(defun get-config-path (config-path)
  "Check the supplied CONFIG-PATH and if one doesn't exist,
use the .fossickerrc in user home or lastly in repo."
  (let ((home-config (cl-fad:merge-pathnames-as-file
                      (user-homedir-pathname) ".fossickerrc"))
        (repo-config (cl-fad:merge-pathnames-as-file
                      fossicker-conf:*basedir* ".fossickerrc")))
    (if config-path
        (if (not (cl-fad:directory-exists-p (pathname config-path)))
            (progn
              (message "Config file save location set to ~a.~%" config-path)
              (setf *config-path* (cl-fad:pathname-as-file config-path)))
            (progn
              (message "~a is a directory, using ~a as config save location instead.~%"
                       config-path
                       home-config)
              (setf *config-path* home-config)))
        (progn
          (message "Config file save location set to ~a~%" home-config)
          (setf *config-path* home-config)))
    (cond ((and config-path (cl-fad:file-exists-p (pathname config-path)))
           (pathname config-path))
          ((cl-fad:file-exists-p home-config) home-config)
          (t repo-config))))

(defun load-config (&optional config-path)
  "Find and load the fossicker configuration."
  (with-open-file (in (get-config-path config-path) :external-format :utf-8)
                  (setf *config* (read in)))
  (setf fossicker-conf:*basedir*
        (or (getf *config* :base-path)
            fossicker-conf:*basedir*))
  ;; load libs
  (load-projects)
  (if (getf *config* :default)
      (set-project (getf *config* :default))
    (auto-select-project)))


;;; Fossicker Libs

(defun load-libs (&rest libraries)
  "If supplied, load LIBS, else load libs supplied in LIBS variable."
  (let ((libs (or libraries (getf *config* :libs))))
    (when libs
      (dolist (lib
               libs
               (message "Fossicker libraries loaded: ~a.~%"
                        libs))
        (require lib)))))


;;; Settings

;;;; Fossicker Types

(defvar *type-registry* nil)

(defun ignore-function (&rest args)
  "Default argument for register-type FUNCTION parameter."
  nil)

(defun get-types ()
  (remove-duplicates *type-registry*
                     :key #'car
                     :from-end t))

(defun register-type (name override &key regexp (function #'ignore-function) formats)
  "Register a new fossicker type. 
Fossicker TYPE is determined according a :REGEXP,
usually matching file extensions.
The asset picked from fossicker data path is
processed by :FUNCTION in order to fit
the type specification.
:FORMATS defines which file format to pick
among possible matches in the data path."
  (check-type regexp list)
  (check-type function cl:function)
  (check-type formats (or boolean list cl:function))
  (when (or override (null (assoc name *type-registry*)))
    (push (list name regexp function formats) *type-registry*)))


;;;; Fossicker Projects

(defun add-project (path)
  "Add path to projects if not already added."
  (pushnew path (getf *config* :projects) :test #'string=))

(defun remove-project (path)
  "Remove path from projects if not already added."
  (delete path (getf *config* :projects) :test #'string=))

(defvar *project-definitions* nil
  "The list of fossicker project definitions.")

(defun get-data-from-file (path)
  "Read s-expression from PATH."
  (assert (cl-fad:file-exists-p path) nil "File ~a doesn't exist." path)
  (with-open-file (in path :external-format :utf-8)
    (read in)))

(defun load-projects ()
  "Loads all projects in PROJECTS."
  (setf *project-definitions* nil)
  (dolist (path (getf *config* :projects))
    (push (get-data-from-file path)
          *project-definitions*)))

(defun projects-assert ()
  (assert *project-definitions* nil
          "No fossicker projects defined. You need at least one."))

;;; Implementation

;;;; Fossicker Project Setting

(defvar *project* nil
  "Name of the fossicker project buffer belongs to.")

(defun project-path-p (project-path)
  (when *default-pathname-defaults*
    (let ((project
            (namestring
             (truename (cl-fad:pathname-as-file project-path))))
          (current
            (namestring (truename *default-pathname-defaults*))))
      (cl-ppcre:scan
       (cl-ppcre:create-scanner (format nil "^~a" project)
                                :case-insensitive-mode t) current))))

(defun find-project (projects)
  (when projects
    (let ((proj (car projects)))
      (if (project-path-p (cadr proj))
          proj
          (find-project (cdr projects))))))

(defun get-project ()
  (assoc *project* *project-definitions* :test #'string=))

(defun show-current-project ()
  "Shows the current fossicker project in minibuffer."
  (message "Fossicker Project currently set to ~a.~%" (or *project* "nothing")))

(defun set-project (project)
  "Manually select a project among fossicker projects list."
  (projects-assert)
  (assert (member project
                  *project-definitions*
                  :key #'car
                  :test #'string=)
          nil "~a is not in project list." project)
  (setf *project* project)
  (show-current-project))

(defun unset-project ()
  "Set project to nil."
  (setf *project* nil)
  (show-current-project))

(defun auto-select-project ()
  "Automatically select a project among fossicker projects list.
Checks the project root of each fossicker project against
the current working directory path to find the project buffer
belongs to."
  (projects-assert)
  (setf *project* (car (find-project *project-definitions*)))
  (show-current-project))


;;;; Asset Generation

(defun type-match-p (fname type)
  (some
   (lambda (regexp)
     (cl-ppcre:scan (cl-ppcre:create-scanner
                     regexp
                     :case-insensitive-mode t) fname))
   (elt type 1)))

(defun matching-types (fname)
  (mapcar #'car
          (remove-if-not
           (lambda (type)
             (type-match-p fname type))
           (get-types))))

(defun matching-spec (types specs)
  (when specs
    (if (member (car specs) types)
        (car specs)
        (matching-spec types (cdr specs)))))

(defun map-to-vein (string legend)
  (when legend
    (cons (cons (cl-ppcre:scan (caar legend) string)
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
      (let ((ndir (cl-fad:merge-pathnames-as-directory
                   (cl-fad:pathname-as-directory dir)
                   (cl-fad:pathname-as-directory (car map)))))
        (prospect
         (cdr map)
         (or (cl-fad:directory-exists-p ndir)
             (cl-fad:pathname-as-directory dir))
         formats
         (or (find-if (lambda (file)
                        (cl-ppcre:scan
                         (format nil "~a\\.(~{~a~^|~})" (car map) formats)
                         (file-namestring file)))
                      (cl-fad:list-directory dir))
             prospect)))
      prospect))

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
  (cl-fad:pathname-as-directory
   (let ((context (cl-fad:pathname-directory-pathname filename)))
     (if (y-or-n-p "Context: ~a" context)
         context
         (pathname (prompt-read "Context"))))))

(defun add-case-variations (formats)
  (apply #'append
         (mapcar (lambda (elt)
                   (list (string-downcase elt)
                         (string-upcase elt)))
                 formats)))

(defun get-extension-list (type ext)
  (let* ((formats (elt (assoc type *type-registry*) 3)))
    (if formats
        (if (functionp formats)
            (funcall formats ext)
            (list ext))
        nil)))

(defun compile-path (spec)
  (cl-fad:canonical-pathname
   (cl-fad:merge-pathnames-as-directory
    (cl-fad:pathname-as-directory (elt (get-project) 1))
    (cl-fad:pathname-as-directory (elt (get-project) 2))
    (cl-fad:pathname-as-directory (or (car spec) "")))))

(defun report (result)
  (message (if (listp result)
               (format nil "~a assets generated!~%"
                       (if result (length result) "No"))
               "Finished!~%")))

(defun generate (filename)
  "Generates the asset according to the double-quoted text
at current cursor position."
  (assert (stringp filename) nil "~a is not a filename." filename)
  (assert *project* nil "No fossicker project selected for current buffer.")
  (let* ((fname (pathname filename))
         (ext (pathname-type fname))
         (types (matching-types (namestring fname)))
         (specs (cdddr (get-project)))
         (type (matching-spec types (mapcar #'car specs)))
         (spec (cdr (assoc type specs)))
         (fn (elt (assoc type *type-registry*) 2))
         (formats (get-extension-list type ext))
         (context (prompt-context fname))
         (path (compile-path spec))
         source)
    (assert types nil
            "Couldn't match file name ~a to regexp list of any fossicker type."
            fname)
    (assert type nil "No matching type is included in project. Possible types: ~a" types)
    (assert (listp formats) nil "Source dispatch function didn't return a list.")
    (setf source (prompt-source
                  (prospect
                   (generate-vein-map (namestring fname) type)
                   (if (getf *config* :data-path)
                       (cl-fad:pathname-as-directory (getf *config* :data-path))
                     (cl-fad:merge-pathnames-as-directory
                      fossicker-conf:*basedir*
                      "data/"))
                   (add-case-variations formats))))
    (assert (cl-fad:file-exists-p source) nil
            "Source ~a is not a regular file." source)
    (assert
     (or (null formats) (cl-ppcre:scan (format nil "\\.(~{~a~^|~})" formats)
                                       (file-namestring source)))
     nil "Source expected to be one of following formats: ~a. Got ~a."
     formats (pathname-type source))
    (report
     (funcall fn path context
              (file-namestring fname)
              ext (cdr spec) source))))
