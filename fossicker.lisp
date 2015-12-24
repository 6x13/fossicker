;;; fossicker.el --- Fossicker: On-the-fly asset generation for development.

;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 23 October 2015
;; Version: 0.1.1
;; Keywords: gamedev, game, development, sprite, asset, tools
;; Homepage: http://kenanb.com

;; This file is not part of GNU Emacs.

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

;;; Code:

;;; Dependencies

(in-package :fossicker)

;;; Fossicker Message

; REVISED
(defun message (&rest args)
  (apply #'format (cons t args)))


;;; Fossicker Libs

; REVISED
(defvar *path* nil
  "Directory containing the Fossicker package.
This is used to load the supporting fossicker type libraries.
The default value is automatically computed from the location of
the Emacs Lisp package.")
(setq path (file-name-directory (or load-file-name buffer-file-name)))
(pushnew (expand-file-name path)
         load-path
         :test 'string=)
(pushnew (expand-file-name "lib/" path)
         load-path
         :test 'string=)

(defvar *libs* '(all)
  "A list of packages to load with FOSSICKER.
Defaults to ALL meta-package.")

(defun load-libs (&rest libraries)
  "If supplied, load LIBS, else load libs supplied in LIBS variable."
  (let ((libs (or libraries *libs*)))
    (when libs
      (dolist (lib
               libs
               (message "Fossicker libraries loaded: %S"
                        libs))
        (require lib)))))


;;; Settings

;;;; Fossicker Data Path

(defvar *data-path*
  (file-name-as-directory (expand-file-name "data/" path))
  "Location of the fossicker data.")


;;;; Fossicker Types

; REVISED
(defvar *type-registry* nil)

; REVISED
(defun ignore ()
  "Default argument for register-type FN parameter."
  nil)

; REVISED
(defun get-types ()
  (remove-duplicates *type-registry*
                     :key #'car
                     :from-end t))

; REVISED
(defun register-type (name override &key regexp (function #'ignore) formats)
  "Register a new fossicker type. 
Fossicker TYPE is determined according a :REGEXP,
usually matching file extensions.
The asset picked from fossicker data path is
processed by :FUNCTION in order to fit
the type specification.
:FORMATS defines which file format to pick
among possible matches in the data path."
  (check-type regexp list)
  (check-type function function)
  (check-type formats (or boolean list))
  (when (or override (null (assoc name *type-registry*)))
    (push (list name regexp function formats) *type-registry*)))


;;;; Fossicker Vein Mappings

; REVISED
(defvar *legend*
  '(("_b_" "button")
    ("_n_" "normal")
    ("_p_" "pressed")
    ("_e_" "enabled"))
  "List of regular expressions and the directory names they map to.")


;;;; Fossicker Projects

; REVISED
(defvar *projects* nil
  "The list of fossicker project paths.")

; REVISED
(defun add-project (path)
  "Add path to *PROJECTS* if not already added."
  (pushnew path *projects* :test #'string=))

; REVISED
(defun remove-project (path)
  "Add path to *PROJECTS* if not already added."
  (delete path *projects* :test #'string=))

; TESTING
(add-project "~/dev/lisp/local-projects/fossicker/test/6x13.lisp")
(add-project "~/dev/lisp/local-projects/fossicker/test/test.lisp")
(remove-project "~/dev/lisp/local-projects/fossicker/test/6x13.lisp")

; REVISED
(defvar *project-definitions* nil
  "The list of fossicker project definitions.")

; REVISED
(defun get-data-from-file (path)
  "Read s-expression from PATH."
  (assert (cl-fad:file-exists-p path) nil "File ~a doesn't exist." path)
  (with-open-file (in path :external-format :utf-8)
    (read in)))

; REVISED
(defun load-projects ()
  "Loads all projects in PROJECTS."
  (setq *project-definitions* nil)
  (dolist (path *projects*)
    (push (get-data-from-file path)
          *project-definitions*)))

; REVISED
(defun projects-assert ()
  (assert *project-definitions* nil
          "No fossicker projects defined. You need at least one."))

;;; Implementation

;;;; Fossicker Project Setting

; REVISED
(defvar *project* nil
  "Name of the fossicker project buffer belongs to.")

; REVISED
(defun get-project ()
  (assoc *project* project-definitions))

; REVISED
(defun show-current-project ()
  "Shows the current fossicker project in minibuffer."
  (message "Fossicker Project currently set to ~a." (or *project* "nothing")))

; REVISED
(defun set-project (project)
  "Manually select a project among fossicker projects list."
  (projects-assert)
  (assert (member project
                  *project-definitions*
                  :key #'car
                  :test #'string=)
          nil "~a is not in project list." project)
  (setq *project* project)
  (show-current-project))

; REVISED
(defun unset-project ()
  "Set project to nil."
  (setq *project* nil)
  (show-current-project))


;;;; Asset Generation

; REVISED
(defun type-match-p (fname type)
  (some
   (lambda (regexp)
     (cl-ppcre:scan regexp fname))
   (elt type 1)))

; REVISED
(defun matching-types (fname)
  (mapcar #'car
          (remove-if-not
           (lambda (type)
             (type-match-p fname type))
           (get-types))))

; REVISED
(defun matching-spec (types specs)
  (when specs
    (if (member (car specs) types)
        (car specs)
        (matching-spec types (cdr specs)))))

; REVISED
(defun map-to-vein (string legend)
  (when legend
    (cons (cons (cl-ppcre:scan (caar legend) string)
                (cdar legend))
          (map-to-vein string (cdr legend)))))

; REVISED
(defun generate-vein-map (fname atype)
  (cons (symbol-name atype)
        (apply #'append
               (mapcar #'cdr
                       (sort 
                        (delete-if #'null
                                   (map-to-vein fname (copy-alist legend))
                                   :key #'car)
                        #'< :key #'car)))))

(defun prospect (map dir formats &optional prospect)
  (if map
      (let ((ndir (concat (file-name-as-directory dir) (car map))))
        (prospect
         (cdr map)
         (if (file-exists-p ndir)
             ndir
             (file-name-as-directory dir))
         formats
         (or (car (directory-files
                   dir t
                   (concat (car map) "\\." (regexp-opt formats))))
             prospect)))
      prospect))

(defun prompt-source (prospect)
  (expand-file-name
   (if prospect
       (read-file-name "Source: "
                       (expand-file-name
                        (file-name-directory prospect))
                       nil t
                       (file-name-nondirectory prospect))
       (read-file-name "Source: "
                       (expand-file-name
                        data-path)
                       nil t))))

(defun prompt-context (filename)
  (file-name-as-directory
   (read-string "Context: "
                (file-name-directory filename))))

(defun add-case-variations (formats)
  (apply #'append
         (mapcar (lambda (elt)
                   (list (downcase elt)
                         (upcase elt)))
                 formats)))

(defun get-extension-list (type ext)
  (let* ((formats (elt (assoc type type-registry) 3)))
    (if formats
        (if (functionp formats)
            (funcall formats ext)
            (list ext))
        nil)))

(defun compile-path (spec)
  (concat (file-name-as-directory (elt (get-project) 1))
          (file-name-as-directory (elt (get-project) 2))
          (file-name-as-directory (or (car spec) ""))))

(defun report (result)
  (message (if (listp result)
               (format "%s assets generated!"
                       (if result (length result) "No"))
               "Finished!")))

(defun generate (&optional filename)
  "Generates the asset according to the double-quoted text
at current cursor position."
  (assert (or (null filename)
              (stringp filename)) nil "%S is not a filename." filename)
  (assert project nil "No fossicker project selected for current buffer.")
  (let* ((fname (or filename (get-text-inside-quotes)))
         (ext (file-name-extension fname nil))
         (types (matching-types fname))
         (specs (cdddr (get-project)))
         (type (matching-spec types (mapcar #'car specs)))
         (spec (cdr (assoc type specs)))
         (fn (elt (assoc type type-registry) 2))
         (formats (get-extension-list type ext))
         (context (prompt-context fname))
         (path (compile-path spec))
         source)
    (assert types nil
            "Couldn't match file name %S to regexp list of any fossicker type."
            fname)
    (assert type nil "No matching type is included in project. Possible types: %S" types)
    (assert (listp formats) nil "Source dispatch function didn't return a list.")
    (setq source (prompt-source
                  (prospect
                   (generate-vein-map fname type)
                   (file-name-as-directory data-path)
                   (add-case-variations formats))))
    (assert (file-regular-p source) nil
            "Source %S is not a regular file." source)
    (assert
     (or (null formats) (string-match (concat "\\." (regexp-opt formats) "\\'") source))
     nil "Source expected to be one of following formats: %S. Got %S."
     formats (file-name-extension source))
    (report
     (funcall fn path context
              (file-name-nondirectory fname)
              ext (cdr spec) source))))
