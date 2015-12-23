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

(require 'cl-lib)


;;; Fossicker Group

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


;;; Fossicker Message

(defun fossicker--message (&rest args)
  (apply 'message args))


;;; Fossicker Libs

(defvar fossicker--path nil
  "Directory containing the Fossicker package.
This is used to load the supporting fossicker type libraries.
The default value is automatically computed from the location of
the Emacs Lisp package.")
(setq fossicker--path (file-name-directory (or load-file-name buffer-file-name)))
(cl-pushnew (expand-file-name fossicker--path)
            load-path
            :test 'string=)
(cl-pushnew (expand-file-name "lib/" fossicker--path)
            load-path
            :test 'string=)

;;;###autoload
(defvar fossicker-libs '(fossicker-all)
  "A list of lib packages to load with FOSSICKER.
Defaults to FOSSICKER-ALL meta-package.")

;;;###autoload
(defun fossicker-load-libs (&rest libs)
  "If supplied, load LIBS, else load libs supplied in FOSSICKER-LIBS variable."
  (let ((ctr (or libs fossicker-libs)))
    (when ctr
      (dolist (lib
               ctr
               (fossicker--message "Fossicker libraries loaded: %S"
                        ctr))
        (require lib)))))


;;; Settings

;;;; Fossicker Data Path

(defcustom fossicker-data-path
  (file-name-as-directory (expand-file-name "data/" fossicker--path))
  "Location of the fossicker data."
  :group 'fossicker
  :type '(directory :tag "Path"
                    :size 30
                    :format "%t: %v\n"))


;;;; Fossicker Types

(defvar fossicker--type-registry nil)

(defun fossicker--get-types ()
  (cl-remove-duplicates fossicker--type-registry
                        :key #'car
                        :from-end t))

;;;###autoload
(defun fossicker-register-type (name override &rest args)
  "Register a new fossicker type. 
Fossicker TYPE is determined according a :REGEXP,
usually matching file extensions.
The asset picked from fossicker data path is
processed by :FUNCTION in order to fit
the type specification.
:FORMATS defines which file format to pick
among possible matches in the data path.
You can add customizations parameeters
to type using :WIDGETS."
  (let ((regexp (plist-get args :regexp))
        (fn (or (plist-get args :function) 'ignore))
        (formats (plist-get args :formats))
        (widgets (plist-get args :widgets)))
    (cl-check-type regexp list)
    (cl-check-type fn function)
    (cl-check-type formats (or boolean list))
    (cl-check-type widgets list)
    (when (or override (null (assq name fossicker--type-registry)))
      (push (list name regexp fn formats widgets) fossicker--type-registry))))


;;;; Fossicker Vein Mappings

;;;###autoload
(defcustom fossicker-legend
  '(("_b_" "button")
    ("_n_" "normal")
    ("_p_" "pressed")
    ("_e_" "enabled"))
  "List of regular expressions and the directory names they map to."
  :group 'fossicker
  :type '(repeat (cons :format "%v"
                       (regexp :size 24
                               :format "%t: %v\n")
                       (repeat :tag "Map To" :value ("")
                               (string :size 20
                                       :format "%v\n")))))


;;;; Fossicker Projects

;;;###autoload
(defcustom fossicker-projects
  nil
  "The list of fossicker project paths."
  :group 'fossicker
  :type '(repeat :tag "Fossicker Projects"
                 (file :must-match t
                       :tag "Project File")))

(defvar fossicker--project-definitions
  nil
  "The list of fossicker project definitions.")

(defun fossicker--get-data-from-file (path)
  "Read s-expression from PATH."
  (read (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))))

;;;###autoload
(defun fossicker-load-projects ()
  "Loads all projects in FOSSICKER-PROJECTS."
  (setq fossicker--project-definitions nil)
  (dolist (path fossicker-projects)
    (push (fossicker--get-data-from-file path)
          fossicker--project-definitions)))

(defun fossicker--projects-assert ()
  (cl-assert fossicker--project-definitions nil
             "No fossicker projects defined. You need at least one."))


;;; Implementation

;;;; Fossicker Project Setting

(defvar-local fossicker-project nil
  "Name of the fossicker project buffer belongs to.")

(defun fossicker--project-file-p (projectpath)
  (when (buffer-file-name) 
    (let ((pp (directory-file-name (expand-file-name projectpath)))
          (bp (file-name-directory (buffer-file-name))))
      (string-prefix-p pp bp))))

(defun fossicker--find-project (projects)
  (when projects
    (let ((proj (car projects)))
      (if (fossicker--project-file-p (cdr (assoc 'root proj)))
          proj
        (fossicker--find-project (cdr projects))))))

(defun fossicker-show-current-project ()
  "Shows the current fossicker project in minibuffer."
  (interactive)
  (fossicker--message "Fossicker Project currently set to %s." (or fossicker-project "nothing")))

;;;###autoload
(defun fossicker-customize ()
  "Customize fossicker group."
  (interactive)
  (customize-group 'fossicker))

;;;###autoload
(defun fossicker-set-project (&optional project)
  "Manually select a project among fossicker projects list."
  (interactive)
  (fossicker--projects-assert)
  (cl-assert (or
              (null project)
              (member project (mapcar 'car fossicker--project-definitions)))
             nil "%S is not in project list." project)
  (setq fossicker-project (or project
                              (completing-read
                               "Select Fossicker Project buffer belongs to: "
                               (mapcar 'car fossicker--project-definitions)
                               nil t)))
  (fossicker-show-current-project))

;;;###autoload
(defun fossicker-unset-project ()
  "Set fossicker-project to nil."
  (interactive)
  (setq fossicker-project nil)
  (fossicker-show-current-project))

;;;###autoload
(defun fossicker-auto-select-project ()
  "Automatically select a project among fossicker projects list.
Checks the project root of each fossicker project against
the current buffer path to find the project buffer
belongs to."  
  (interactive)
  (fossicker--projects-assert)
  (setq fossicker-project (car (fossicker--find-project
                                fossicker--project-definitions)))
  (fossicker-show-current-project))


;;;; Asset Generation

(defun fossicker--project-get (data)
  (cdr (assq data
             (cdr
              (assoc
               fossicker-project
               fossicker--project-definitions)))))

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

(defun fossicker--type-match-p (fname type)
  (cl-some
   (lambda (regexp)
     (string-match regexp fname))
   (elt type 1)))

(defun fossicker--matching-types (fname)
  (mapcar 'car
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
               (mapcar (lambda (x) (cdr x))
                       (sort 
                        (assq-delete-all nil (fossicker--map-to-vein
                                              fname
                                              (copy-alist
                                               fossicker-legend)))
                        (lambda (l1 l2) (< (car l1) (car l2))))))))

(defun fossicker--prospect (map dir extl &optional prospect)
  (if map
      (let ((ndir (concat (file-name-as-directory dir) (car map))))
        (fossicker--prospect
         (cdr map)
         (if (file-exists-p ndir)
             ndir
           (file-name-as-directory dir))
         extl
         (or (car (directory-files
                   dir t
                   (concat (car map) "\\." (regexp-opt extl))))
             prospect)))
    prospect))

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

(defun fossicker--add-case-variations (extl)
  (apply 'append
         (mapcar (lambda (elt)
                   (list (downcase elt)
                         (upcase elt)))
                 extl)))

(defun fossicker--get-extension-list (type ext)
  (let* ((formats (elt (assoc type fossicker--type-registry) 3)))
    (if formats
        (if (functionp formats)
            (funcall formats ext)
          (list ext))
      nil)))

(defun fossicker--compile-path (spec)
  (concat (file-name-as-directory (fossicker--project-get 'root))
          (file-name-as-directory (fossicker--project-get 'path))
          (file-name-as-directory (or (car spec) ""))))

(defun fossicker--report (result)
  (fossicker--message (if (listp result)
               (format "%s assets generated!"
                       (if result (length result) "No"))
             "Finished!")))

;;;###autoload
(defun fossicker-generate (&optional filename)
  "Generates the asset according to the double-quoted text
at current cursor position."
  (interactive)
  (cl-assert (or (null filename)
                 (stringp filename)) nil "%S is not a filename." filename)
  (cl-assert fossicker-project nil "No fossicker project selected for current buffer.")
  (let* ((fname (or filename (fossicker--get-text-inside-quotes)))
         (ext (file-name-extension fname nil))
         (types (fossicker--matching-types fname))
         (specs (fossicker--project-get 'spec))
         (type (fossicker--matching-spec types (mapcar 'car specs)))
         (spec (cdr (assq type specs)))
         (fn (elt (assoc type fossicker--type-registry) 2))
         (formats (fossicker--get-extension-list type ext))
         (context (fossicker--prompt-context fname))
         (path (fossicker--compile-path spec))
         source)
    (cl-assert types nil
               "Couldn't match file name %S to regexp list of any fossicker type."
               fname)
    (cl-assert type nil "No matching type is included in project. Possible types: %S" types)
    (cl-assert (listp formats) nil "Source dispatch function didn't return a list.")
    (setq source (fossicker--prompt-source
                  (fossicker--prospect
                   (fossicker--generate-vein-map fname type)
                   (file-name-as-directory fossicker-data-path)
                   (fossicker--add-case-variations formats))))
    (cl-assert (file-regular-p source) nil
               "Source %S is not a regular file." source)
    (cl-assert
     (or (null formats) (string-match (concat "\\." (regexp-opt formats) "\\'") source))
     nil "Source expected to be one of following formats: %S. Got %S."
     formats (file-name-extension source))
    (fossicker--report
     (funcall fn path context
              (file-name-nondirectory fname)
              ext (cdr spec) source))))


;;; Fossicker Minor Mode

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


;;; Fossicker Provide

(provide 'fossicker)

;;; fossicker.el ends here
