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


;;;;;;;;;;;;;;;;
;;; Dependencies
;;
;;

(require 'cl-lib)
(require 'cl-extra)
(require 'wid-edit)
(require 'widget)
(require 'cus-edit)
(require 'info)


;;;;;;;;
;;; Path
;;
;;

(defvar fossicker--path nil
  "Directory containing  the Fossicker  package. This is  used to
  load the supporting fossicker type libraries. The default value
  is automatically computed  from the location of  the Emacs Lisp
  package.")

(setq fossicker--path
      (file-name-as-directory
       (file-name-directory
        (or load-file-name buffer-file-name))))

(cl-pushnew (expand-file-name "lib/" fossicker--path)
            load-path
            :test 'string=)



;;;;;;;;;
;;; Group
;;
;;

;;;###autoload
(defgroup fossicker nil
  "On-the-fly asset generation for development."
  :tag "Fossicker"
  :prefix "fossicker-"
  :group 'extensions
  :group 'convenience
  :link '(url-link :tag "Website" ...)
  :link '(url-link :tag "Libraries by 6x13" ...)
  :link '(url-link :tag "Libraries by Kenan Bölükbaşı" ...)
  :link '(url-link :tag "Download" ...)
  :link '(url-link :tag "Description" ...)
  :link '(url-link :tag "Send Bug Report" ...))

(defcustom fossicker-data-path
  (file-name-as-directory (expand-file-name "data/" fossicker--path))
  "Location of the fossicker data."
  :group 'fossicker
  :type '(directory :tag "Path"
                    :size 30
                    :format "%t: %v\n"))

;;;###autoload
(defcustom fossicker-legend
  '(("_b_" "button")
    ("_n_" "normal")
    ("_p_" "pressed")
    ("_e_" "enabled"))
  "List of regular  expressions and the directory  names they map
to."
  :group 'fossicker
  :type '(repeat (cons :format "%v"
                       (regexp :size 24
                               :format "%t: %v\n")
                       (repeat :tag "Map To" :value ("")
                               (string :size 20
                                       :format "%v\n")))))

;;;###autoload
(defcustom fossicker-projects
  nil
  "The list of fossicker project paths."
  :group 'fossicker
  :type '(repeat :tag "Fossicker Projects"
                 :offset 9
                 (list :format "%v"
                       (file :must-match t
                             :tag "Project File")
                       (menu-choice :tag "Project Root"
                                    (const :tag "Project File Path" nil)
                                    (directory :must-match t
                                               :menu-tag "Custom Path")))))

;;;###autoload
(defcustom fossicker-libs
  '(fossicker-all)
  "A  list  of  packages  to load  with  FOSSICKER.  Defaults  to
FOSSICKER-ALL meta-package."
  :group 'fossicker
  :type '(repeat :tag "Fossicker Libraries"
                 (symbol :tag "Library Name")))

;;;###autoload
(defun fossicker-customize ()
  "Customize fossicker group."
  (interactive)
  (customize-group 'fossicker))


;;;;;;;;;;;
;;; Utility
;;
;;

(defun fossicker--message (&rest args)
  (apply 'message args))

;;;###autoload
(defun fossicker-load-libs (&rest libraries)
  "If   supplied,  load   LIBS,  else   load  libs   supplied  in
FOSSICKER-LIBS variable."
  (let ((libs (or libraries fossicker-libs)))
    (when libs
      (dolist (lib
               libs
               (fossicker--message "Fossicker libraries loaded: %S"
                                   libs))
        (require lib)))))



;;;;;;;;;
;;; Types
;;
;;

(defvar fossicker--type-registry nil)

;;;###autoload
(defun fossicker-register-type (name override &rest args)
  "Register a  new fossicker  type. Fossicker TYPE  is determined
according a :REGEXP, usually  matching file extensions. The asset
picked  from fossicker  data path  is processed  by :FUNCTION  in
order to fit the type  specification. :FORMATS defines which file
format to pick  among possible matches in the data  path. You can
add customizations parameeters to type using :WIDGETS."
  (let ((regexp (plist-get args :regexp))
        (fn (or (plist-get args :function) 'ignore))
        (formats (plist-get args :formats))
        (widgets (plist-get args :widgets)))
    (cl-check-type regexp list)
    (cl-check-type fn function)
    (cl-check-type formats (or boolean list function))
    (cl-check-type widgets list)
    (when (or override (null (assq name fossicker--type-registry)))
      (push (list name regexp fn formats widgets) fossicker--type-registry))))

;;
;;;; Type Accessors
;;
;;

(defun fossicker--type-name (type)
  (elt type 0))

(defun fossicker--type-regexp (type)
  (elt type 1))

(defun fossicker--type-function (type)
  (elt type 2))

(defun fossicker--type-formats (type)
  (elt type 3))

(defun fossicker--type-widgets (type)
  (elt type 4))

(defun fossicker--get-types ()
  (cl-remove-duplicates fossicker--type-registry
                        :key #'fossicker--type-name
                        :from-end t))


;;;;;;;;;;;;
;;; Projects
;;
;;
;;;; Load Projects
;;
;;

(defvar fossicker--project-registry
  nil
  "The list of fossicker project definitions.")

(defvar-local fossicker-project nil
  "Name of the fossicker project buffer belongs to.")

(defun fossicker--get-data-from-file (path)
  "Read s-expression from PATH."
  (read (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))))

;;;###autoload
(defun fossicker-load-projects ()
  "Loads all projects in FOSSICKER-PROJECTS."
  (setq fossicker--project-registry nil)
  (dolist (proj fossicker-projects)
    (let ((data (fossicker--get-data-from-file (car proj))))
      (push (cons
             (car data)
             (cons
              (or (cadr proj)
                  (file-name-directory (car proj)))
              (cdr data)))
            fossicker--project-registry))))

;;
;;;; Project Accessors
;;
;;

(defun fossicker--get-project ()
  (assoc fossicker-project fossicker--project-registry))

(defun fossicker--project-name (project)
  (elt project 0))

(defun fossicker--project-root (project)
  (elt project 1))

(defun fossicker--project-path (project)
  (elt project 2))

(defun fossicker--project-specs (project)
  (cl-cdddr project))

;;
;;;; Current Project
;;
;;

(defun fossicker-show-current-project ()
  "Shows the current fossicker project in minibuffer."
  (interactive)
  (fossicker--message "Fossicker Project currently set to %s."
                      (or fossicker-project "nothing")))

;;
;;;; Selection
;;
;;

(defun fossicker--projects-assert ()
  (cl-assert fossicker--project-registry nil
             "No fossicker projects defined. You need at least one."))

;;;###autoload
(defun fossicker-set-project (&optional project)
  "Manually select a project among fossicker projects list."
  (interactive)
  (fossicker--projects-assert)
  (cl-assert (or
              (null project)
              (member project (mapcar 'fossicker--project-name
                                      fossicker--project-registry)))
             nil "%S is not in project list." project)
  (setq fossicker-project (or project
                              (completing-read
                               "Select Fossicker Project buffer belongs to: "
                               (mapcar 'fossicker--project-name
                                       fossicker--project-registry)
                               nil t)))
  (fossicker-show-current-project))

;;;###autoload
(defun fossicker-unset-project ()
  "Set fossicker-project to nil."
  (interactive)
  (setq fossicker-project nil)
  (fossicker-show-current-project))

;;
;;;; Auto-Selection
;;
;;

(defun fossicker--project-file-p (projectpath)
  (when (buffer-file-name) 
    (let ((pp (directory-file-name (expand-file-name projectpath)))
          (bp (file-name-directory (buffer-file-name))))
      (string-prefix-p pp bp))))

(defun fossicker--find-project (projects)
  (when projects
    (let ((proj (car projects)))
      (if (fossicker--project-file-p (fossicker--project-root proj))
          proj
        (fossicker--find-project (cdr projects))))))

;;;###autoload
(defun fossicker-auto-select-project ()
  "Automatically  select  a   project  among  fossicker  projects
list. Checks the  project root of each  fossicker project against
the current buffer path to find the project buffer belongs to."
  (interactive)
  (fossicker--projects-assert)
  (setq fossicker-project (fossicker--project-name
                           (fossicker--find-project
                            fossicker--project-registry)))
  (fossicker-show-current-project))



;;;;;;;;;;;;
;;; Prospect
;;
;;

(defun fossicker--type-match-p (fname type)
  (cl-some
   (lambda (regexp)
     (string-match regexp fname))
   (fossicker--type-regexp type)))

(defun fossicker--matching-types (fname)
  (mapcar 'fossicker--type-name
          (cl-remove-if-not
           (lambda (type)
             (fossicker--type-match-p fname type))
           (fossicker--get-types))))

(defun fossicker--matching-spec (types specs)
  (when specs
    (if (member (car specs) types)
        (car specs)
      (fossicker--matching-spec types (cdr specs)))))

(defun fossicker--map-to-vein (string legend)
  (when legend
    (cons (cons (string-match (caar legend) string)
                (cdar legend))
          (fossicker--map-to-vein string (cdr legend)))))

(defun fossicker--generate-vein-map (fname atype)
  (cons (symbol-name atype)
        (apply 'append
               (mapcar 'cdr
                       (cl-sort 
                        (assq-delete-all nil (fossicker--map-to-vein
                                              fname
                                              (copy-alist
                                               fossicker-legend)))
                        '< :key 'car)))))

(defun fossicker--prospect (map dir formats &optional prospect)
  (if map
      (let ((ndir (concat (file-name-as-directory dir) (car map))))
        (fossicker--prospect
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

(defun fossicker--add-case-variations (formats)
  (apply 'append
         (mapcar (lambda (elt)
                   (list (downcase elt)
                         (upcase elt)))
                 formats)))

(defun fossicker--get-extension-list (type ext)
  (let* ((formats (fossicker--type-formats
                   (assoc type fossicker--type-registry))))
    (if formats
        (if (functionp formats)
            (funcall formats ext)
          (list ext))
      nil)))

(defun fossicker--compile-path (spec)
  (concat (file-name-as-directory (fossicker--project-root (fossicker--get-project)))
          (file-name-as-directory (fossicker--project-path (fossicker--get-project)))
          (file-name-as-directory (or (car spec) ""))))

(defun fossicker--report (result)
  (fossicker--message (if (listp result)
                          (format "%s assets generated!"
                                  (if result (length result) "No"))
                        "Finished!")))

;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minibuffer Interface
;;
;;

(defun fossicker--get-text-inside-quotes ()
  "Return text between double straight
quotes on each side of cursor."
  (let (p0 p1 p2)
    (setq p0 (point))
    (skip-chars-backward "^\"\'")
    (setq p1 (point))
    (skip-chars-forward "^\"\'")
    (setq p2 (point))
    (goto-char p0)
    (buffer-substring-no-properties p1 p2)))

(defun fossicker--prompt-source (prospect)
  (expand-file-name
   (if prospect
       (read-file-name "Source: "
                       (expand-file-name
                        (file-name-directory prospect))
                       nil t
                       (file-name-nondirectory prospect))
     (read-file-name "Source: "
                     (expand-file-name
                      fossicker-data-path)
                     nil t))))

(defun fossicker--prompt-context (filename)
  (file-name-as-directory
   (read-string "Context: "
                (file-name-directory filename))))

;;;###autoload
(defun fossicker-generate (&optional filename)
  "Generates  the asset  according to  the double-quoted  text at
current cursor position."
  (interactive)
  (cl-assert (or (null filename) (stringp filename)) nil
             "%S is not a filename." filename)
  (cl-assert fossicker-project nil
             "No fossicker project selected for current buffer.")
  (let* ((fname (or filename (fossicker--get-text-inside-quotes)))
         (ext (file-name-extension fname nil))
         (types (fossicker--matching-types fname))
         (specs (fossicker--project-specs (fossicker--get-project)))
         (type (fossicker--matching-spec types (mapcar 'car specs)))
         (spec (cdr (assq type specs)))
         (fn (fossicker--type-function (assoc type fossicker--type-registry)))
         (formats (fossicker--get-extension-list type ext))
         (context (fossicker--prompt-context fname))
         (path (fossicker--compile-path spec))
         source)
    (cl-assert types nil
               "Couldn't match file name %S to regexp list of any fossicker type."
               fname)
    (cl-assert type nil
               "No matching type is included in project. Possible types: %S" types)
    (cl-assert (listp formats) nil
               "Source dispatch function didn't return a list.")
    (setq source (fossicker--prompt-source
                  (fossicker--prospect
                   (fossicker--generate-vein-map fname type)
                   (file-name-as-directory fossicker-data-path)
                   (fossicker--add-case-variations formats))))
    (cl-assert (file-regular-p source) nil
               "Source %S is not a regular file." source)
    (cl-assert (or (null formats)
                   (string-match
                    (concat "\\." (regexp-opt formats) "\\'")
                    source))
               nil "Source expected to be one of following formats: %S. Got %S."
               formats (file-name-extension source))
    (fossicker--report
     (funcall fn path context
              (file-name-nondirectory fname)
              ext (cdr spec) source))))

;;
;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;; Fossicker Widget
;;
;;

(defvar fossicker--logo
  (find-image '((:type xpm :file "etc/fossicker-logo.xpm")
                (:type pbm :file "etc/fossicker-logo.pbm"))))

(defvar-local fossicker--source nil
  "Holds the source data.")

(defvar-local fossicker--formats nil
  "Holds the acceptable source format list.")

;;
;;;; Utility
;;
;;

(defun fossicker--widget-message (fn &rest args)
  (if (and (boundp 'fossicker--form) fossicker--form)
      (let ((log (fossicker--fget 'log)))
        (when log
          (save-excursion
            (widget-value-set log (apply 'format args))))
        (widget-setup))
    (apply 'message args)))


;;
;;;; Widget Form
;;
;;

(defvar-local fossicker--form nil
  "Holds the form data.")

(defun fossicker--fget (id)
  (cdr (assoc id fossicker--form)))

(defun fossicker--fset (id widget)
  (let ((old (assoc id fossicker--form)))
    (if old
        (setcdr old widget)
      (push (cons id widget) fossicker--form))))

(defun fossicker--ftag (string)
  (format " %-10s" string))

(defun fossicker--ffmt-src (src)
  (let ((maxlen 50) (len (length src)))
    (if (< len maxlen)
        (format (format " %%-%ss" maxlen) src)
      (concat " ..." (substring src (- len (- maxlen 4))) " "))))

(defun fossicker--ffmt-inf (str)
  (widget-insert (propertize str 'face 'widget-inactive)))

(defun fossicker--ffmt-doc (str)
  (widget-insert (propertize str 'face 'widget-documentation)))

(defun fossicker--ffmt-emp (str)
  (widget-insert (propertize str 'face 'success)))

;;
;;;; Widget Insertions
;;
;;

(defun fossicker--widget-insert-logo ()
  (if fossicker--logo
      (insert-image fossicker--logo)
    (widget-insert
     (propertize "Fossicker" 'face 'info-title-1)
     (propertize " 6x13" 'face '(:foreground "red")))))

(defun fossicker--widget-insert-header ()
  (widget-create 'url-link
                 :tag "Website"
                 :format "%[%t%]  "
                 :button-face 'info-xref
                 "http://www.6x13.com")
  (widget-create 'url-link
                 :tag "Github"
                 :format "%[%t%]  "
                 :button-face 'info-xref
                 "http://www.6x13.com")
  (widget-create 'url-link
                 :tag "Download"
                 :format "%[%t%]  "
                 :button-face 'info-xref
                 "http://www.6x13.com")
  (widget-create 'url-link
                 :tag "Description"
                 :format "%[%t%]  "
                 :button-face 'info-xref
                 "http://www.6x13.com")
  (widget-create 'url-link
                 :tag "Send Bug Report"
                 :format "%[%t%]"
                 :button-face 'info-xref
                 "http://www.6x13.com"))

(defun fossicker--widget-insert-info ()
  (fossicker--ffmt-inf "Select ")
  (fossicker--ffmt-doc "project") 
  (fossicker--ffmt-inf ". Type a ") 
  (fossicker--ffmt-doc "file name") 
  (fossicker--ffmt-inf ". Then press ") 
  (fossicker--ffmt-doc "GENERATE") 
  (fossicker--ffmt-inf ". That's it!\n ")
  (fossicker--ffmt-inf "Check out ")
  (widget-create 'url-link
                 :tag "our website"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]"
                 :button-face 'info-xref
                 "http://www.6x13.com")
  (fossicker--ffmt-inf " and ")
  (widget-create 'url-link
                 :tag "follow us"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]"
                 :button-face 'info-xref
                 "http://www.6x13.com")
  (fossicker--ffmt-inf " on Twitter for more libraries\n and games.\n\n"))

(defun fossicker--widget-list-projects ()
  (let ((value nil)
        (projlist (mapcar 'fossicker--project-name fossicker--project-registry))
        name)
    (dolist (proj projlist)
      (setq name (capitalize proj))
      (cl-pushnew
       (list 'choice-item
             :format "%[%t%]"
             :tag (format "[ %-14s]" name)
             :menu-tag name
             :value proj)
       value))
    (push
     (list 'choice-item
           :format "%[%t%]"
           :tag (format "[ %-14s]" "NONE SELECTED")
           :menu-tag "Select None"
           :value nil)
     value)))

(defun fossicker--widget-create-types ()
  (apply 'widget-create
         'choice
         :size 17
         :format "%v   "
         (mapcar
          (lambda (type)
            (list 'const
                  :format "%t"
                  :tag (format "%-17s"
                               (if type
                                   (capitalize (symbol-name type))
                                 "NONE"))
                  type))
          (cons nil (mapcar 'fossicker--type-name (fossicker--get-types))))))

;;
;;;; Widget Callbacks
;;
;;

(defun fossicker--widget-fname-notify (w &rest ignore)
  (let* ((fname (concat
                 (or (file-name-as-directory
                      (widget-value (fossicker--fget 'context))) "")
                 (or (widget-value w) "")))
         (ext (file-name-extension fname nil))
         (types (fossicker--matching-types fname))
         (specs (fossicker--project-specs (fossicker--get-project)))
         (type (fossicker--matching-spec types (mapcar 'car specs)))
         (spec (cdr (assq type specs)))
         (formats (fossicker--get-extension-list type ext))
         (path (fossicker--compile-path spec))
         prospect)
    (widget-value-set (fossicker--fget 'type) type)
    (if types
        (if type
            (progn
              (setq fossicker--formats formats)
              (fossicker--message
               "SUCCESS:\n Found match for filename %S." fname)
              (widget-apply (fossicker--fget 'gen) :activate)
              (setq prospect (fossicker--prospect
                              (fossicker--generate-vein-map fname type)
                              (file-name-as-directory fossicker-data-path)
                              (fossicker--add-case-variations formats)))
              (when prospect
                (fossicker--message
                 "SUCCESS:\n Found match for filename %S.\n Prospect: %s"
                 fname prospect)
                (widget-value-set (fossicker--fget 'source)
                                  (fossicker--ffmt-src prospect))))
          (progn
            (widget-apply (fossicker--fget 'gen) :deactivate)
            (fossicker--message
             "WARNING:\n No matching type is included in project.\n Possible types: %S"
             types)))   
      (progn
        (widget-apply (fossicker--fget 'gen) :deactivate)
        (fossicker--message
         "WARNING:\n Couldn't match file name %S to regexp list of\n any fossicker type."
         fname)))
    (widget-setup)))

(defun fossicker--widget-fname-action (w &rest ignore)
  (let* ((fname (widget-value w))
         (fl (file-name-nondirectory fname))
         (cn (file-name-directory fname)))
    (when cn
      (widget-value-set (fossicker--fget 'context) cn))
    (widget-value-set w fl)
    (widget-setup)))

(defun fossicker--widget-source-notify (w &rest ignore)
  (let ((src (read-file-name "Source: " "~/dev" nil t)))
    (if (file-regular-p src)
        (if (or (null fossicker--formats)
                (string-match
                 (concat "\\." (regexp-opt fossicker--formats) "\\'")
                 src))
            (progn
              (setq fossicker--source src)
              (widget-value-set w (fossicker--ffmt-src src))
              (fossicker--message
               "SUCCESS:\n Successfully set the source to %S" src))
          (fossicker--message
           "ERROR:\n Source expected to be one of following formats: %S.\n Got %S."
           fossicker--formats (file-name-extension src)))
      (fossicker--message
       "ERROR:\n Source %S is not a regular file." src))))

(defun fossicker--widget-generate-notify (&rest ignore)
  (let* ((fname (file-name-nondirectory
                 (widget-value (fossicker--fget 'fname))))
         (context (or (file-name-directory
                       (widget-value (fossicker--fget 'fname)))
                      (widget-value (fossicker--fget 'context))))
         (ext (file-name-extension fname nil))
         (specs (fossicker--project-specs (fossicker--get-project)))
         (type (widget-value (fossicker--fget 'type)))
         (spec (cdr (assq type specs)))
         (fn (fossicker--type-function (assoc type fossicker--type-registry)))
         (path (fossicker--compile-path spec)))
    (fossicker--report
     (funcall fn path context fname ext (cdr spec) fossicker--source))))

;;
;;;; Widget Definition
;;
;;

;;;###autoload
(defun fossicker ()
  "Create the widgets for asset generation."
  (interactive)
  (switch-to-buffer "*FOSSICKER*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (fossicker-load-libs)
  (fossicker-load-projects)
  
  (widget-insert "\n ")
  (fossicker--widget-insert-logo)
  (widget-insert "\n\n ")
  (fossicker--widget-insert-header)
  (widget-insert "\n\n ")
  (fossicker--widget-insert-info)
  
  (fossicker--fset
   'project
   (widget-create 'menu-choice
                  :tag (fossicker--ftag "Project")
                  :format "%t: %v   "
                  :value nil
                  :help-echo "Select a project."
                  :notify (lambda (w &rest ignore)
                            (if (widget-value w)
                                (progn
                                  (fossicker-set-project (widget-value w))
                                  (widget-apply (fossicker--fget 'fname) :activate)
                                  (widget-apply (fossicker--fget 'context) :activate)
                                  (widget-apply (fossicker--fget 'source) :activate))
                              (progn
                                (fossicker-unset-project)
                                (widget-apply (fossicker--fget 'fname) :deactivate)
                                (widget-apply (fossicker--fget 'context) :activate)
                                (widget-apply (fossicker--fget 'source) :activate)
                                (widget-apply (fossicker--fget 'gen) :deactivate))))
                  :args (fossicker--widget-list-projects)))
  
  (fossicker--fset
   'fname
   (widget-create 'editable-field
                  :size 20
                  :tag (fossicker--ftag "File Name")
                  :format "%t: %v\n"
                  :notify 'fossicker--widget-fname-notify
                  :action 'fossicker--widget-fname-action
                  "test.png"))
  (widget-apply (fossicker--fget 'fname) :deactivate)

  (widget-insert "\n")

  (widget-insert (fossicker--ftag "Type") ": ")

  (fossicker--fset
   'type
   (fossicker--widget-create-types))
  
  (fossicker--fset
   'context
   (widget-create 'editable-field
                  :size 20
                  :tag (fossicker--ftag "Context")
                  :format "%t: %v\n"
                  "ui/buttons/"))
  (widget-apply (fossicker--fget 'context) :deactivate)

  (widget-insert "\n")

  (widget-insert (fossicker--ftag "Source") ": ")

  (fossicker--fset
   'source
   (widget-create 'push-button
                  :size 20
                  :format "%[%v%]"
                  :notify 'fossicker--widget-source-notify
                  (fossicker--ffmt-src "~/")))
  (widget-apply (fossicker--fget 'source) :deactivate)

  (widget-insert "\n\n ")
  
  (widget-create 'push-button
                 :tag "SETTINGS"
                 :format "%[  %t  %]"
                 :button-face 'custom-button
                 :notify (lambda (&rest ignore)
                           (fossicker-customize)))

  (widget-insert "   ")
  (widget-create 'push-button
                 :tag "RESET"
                 :format "%[    %t    %]"
                 :button-face 'custom-button
                 :notify (lambda (&rest ignore)
                           (fossicker)))
  (widget-insert "   ")
  (fossicker--fset
   'gen
   (widget-create 'push-button
                  :tag "GENERATE"
                  :format "%[            %t            %]"
                  :button-face 'custom-button
                  :notify 'fossicker--widget-generate-notify))
  (widget-apply (fossicker--fget 'gen) :deactivate)

  (widget-insert "\n\n")
  
  (fossicker--fset
   'log
   (widget-create 'editable-field
                  :format " %v"
                  :value-face 'default
                  "WELCOME:\n Please select a project first.\n You can define a new project from settings menu."))

  (advice-add 'fossicker--message :around #'fossicker--widget-message)
  (use-local-map widget-keymap)
  (widget-setup))

;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fossicker Project Editor
;;
;;

(defvar-local fossicker--edited-path nil
  "Local variable holding the path of currently edited project.")

(defvar-local fossicker--edited-project nil
  "Local variable holding the value of currently edited project.")

;;
;;;; Type Widgets
;;
;;

(defun fossicker--list-type-widgets ()
  "List of type widgets to choose."
  (let (value)
    (dolist (element
             (fossicker--get-types)
             value)
      (let* ((atsym (fossicker--type-name element))
             (atwidget (fossicker--type-widgets element))
             (atname (symbol-name atsym))
             (wbase (list 'list :tag (format "%-10s" (upcase atname))
                          :format "%t\n%v\n"
                          (list 'const :format "" atsym)
                          (list 'menu-choice :tag "PATH"
                                '(const :tag "Asset Path" nil)
                                (list 'directory
                                      :size 20
                                      :format "%v\n"
                                      :value
                                      (file-name-as-directory atname))))))
        (setq value
              (cons
               (if atwidget (append wbase atwidget) wbase)
               value))))))

(define-widget 'fossicker--type-undefined-widget 'lazy
  "Fallback to sexp when there is no associated type definition."
  :tag "UNDEFINED"
  :format "%t %v\n"
  :type '(sexp :format "%v" :size 20))

;;
;;;; Editor Widget
;;
;;

(defun fossicker--project-generate-notify (&rest ignore)
  (let ((data fossicker--edited-project) ;; So temp-buffer can see data.
        (path fossicker--edited-path))
    (if (and data (y-or-n-p (format "Write project data to %S?" path)))
        (write-region
         (with-temp-buffer
           (cl-prettyprint data)
           (buffer-string))
         nil path nil))))

(defun fossicker--project-widget (file)
  "Create the widgets for fossicker project definition."
  (interactive)
  (switch-to-buffer "*FOSSICKER-EDIT-PROJECT*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (fossicker-load-libs)
  (setq fossicker--edited-path file)
  (if (file-readable-p file)
      (setq fossicker--edited-project (fossicker--get-data-from-file file)))

  (fossicker--widget-insert-logo)
  (widget-insert "\n\n")
  
  (widget-create 'cons
                 :tag "Project"
                 :format "%v"
                 :notify (lambda (w &rest ignore)
                           (setq fossicker--edited-project (widget-value w)))
                 :value fossicker--edited-project
                 '(string :size 41
                          :tag "Project Name "
                          :format "%t: %v\n\n"
                          :value "")
                 (list 'cons :format "%v"
                       '(directory :size 41
                                   :format "%t: %v\n\n"
                                   :tag "Asset Path   "
                                   :value "Resources/")
                       (list 'repeat
                             :tag "Specification"
                             :offset 12
                             (list 'menu-choice
                                   :tag "TYPE"
                                   :args (append
                                          (fossicker--list-type-widgets)
                                          '(fossicker--type-undefined-widget))))))
  
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :tag "RESET"
                 :format "%[          %t          %]"
                 :button-face 'custom-button
                 :notify (lambda (&rest ignore)
                           (fossicker--project-widget fossicker--edited-path)))
  (widget-insert "  ")
  (widget-create 'push-button
                 :tag "GENERATE"
                 :format "%[          %t          %]"
                 :button-face 'custom-button
                 :notify 'fossicker--project-generate-notify)
  (use-local-map widget-keymap)
  (widget-setup))

;;
;;;; Editor Functions
;;
;;

;;;###autoload
(defun fossicker-edit-project (file)
  "Edit one of the loaded Fossicker projects."
  (interactive (list (completing-read
                      "Select project to edit: "
                      fossicker-projects
                      nil t)))
  (if file (fossicker--project-widget file)))

;;;###autoload
(defun fossicker-new-project (file)
  "Create a new Fossicker project."
  (interactive (list (read-file-name "Select project file: ")))
  (fossicker--project-widget file))

;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fossicker Minor Mode
;;
;;

;;;###autoload
(define-minor-mode fossicker-mode
  "Toggle Fossicker mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Fossicker"
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c !") 'fossicker-generate)
            (define-key map (kbd "C-c '") 'fossicker-show-current-project)
            (define-key map [menu-bar fossicker-menu]
              (cons "Fossicker" (make-sparse-keymap)))
            (define-key map [menu-bar fossicker-menu fossicker-settings]
              '("Settings" . fossicker-customize))
            (define-key-after map [menu-bar fossicker-menu fossicker-p-menu]
              (cons "Project" (make-sparse-keymap)) 'fossicker-settings)
            (define-key map [menu-bar fossicker-menu fossicker-p-menu edit]
              '("Edit Project" . fossicker-edit-project))
            (define-key map [menu-bar fossicker-menu fossicker-p-menu new]
              '("New Project" . fossicker-new-project))
            (define-key map [menu-bar fossicker-menu fossicker-p-menu current]
              '("Current" . fossicker-show-current-project))
            (define-key map [menu-bar fossicker-menu fossicker-p-menu unset]
              '("Unset" . fossicker-unset-project))
            (define-key map [menu-bar fossicker-menu fossicker-p-menu select]
              '("Select" . fossicker-set-project))
            (define-key map [menu-bar fossicker-menu fossicker-p-menu autoselect]
              '("Autoselect" . fossicker-auto-select-project))
            (define-key-after map [menu-bar fossicker-menu generate]
              '("Generate" . fossicker-generate) 'fossicker-p-menu)
            (define-key map [tool-bar separator] '(menu-item "--"))
            (define-key-after map [tool-bar fossicker-button]
              (list 'menu-item "Generate Asset" 'fossicker-generate
                    :image (find-image '((:type xpm :file "etc/fossicker-icon.xpm")
                                         (:type pbm :file "etc/fossicker-icon.pbm")))
                    :help "Generate asset from string at current position of cursor")
              'separator)
            map)
  :group 'fossicker
  (fossicker-load-libs)
  (fossicker-load-projects)
  (or fossicker-project (fossicker-auto-select-project)))

;;;###autoload
(define-globalized-minor-mode global-fossicker-mode
  fossicker-mode 
  (lambda () (fossicker-mode t))
  :group 'fossicker)


;;;;;;;;;;;;;;;;;;;;;
;;; Fossicker Provide
;;
;;

(provide 'fossicker)

;;; fossicker.el ends here
