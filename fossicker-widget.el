;;; fossicker-widget.el --- Widget definition for Fossicker Library.

;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 18 December 2015
;; Version: 0.1.1
;; Keywords: gamedev, game, development, sprite, fossicker, asset, tools
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

(require 'fossicker)

(eval-when-compile
  (require 'wid-edit))
(require 'widget)
(require 'cus-edit)
(require 'info)


;;; Utility

(defvar fossicker--logo (find-image '((:type xpm :file "etc/fossicker-logo.xpm")
                                      (:type pbm :file "etc/fossicker-logo.pbm"))))
(defvar-local fossicker--fsrc nil
  "Holds the source data.")

(defvar-local fossicker--fextl nil
  "Holds the extension list.")

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


;;; Widget

;;;; Widget Boilerplate

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


;;;; Widget Functions

(defun fossicker--widget-message (fn &rest args)
  (if (and (boundp 'fossicker--form) fossicker--form)
      (let ((log (fossicker--fget 'log)))
        (when log
          (save-excursion
            (widget-value-set log (apply 'format args))))
        (widget-setup))
    (apply 'message args)))

(defun fossicker--widget-list-projects ()
  (let ((value nil)
        (projlist (mapcar 'car fossicker-projects))
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


;;;; Widget Callbacks

(defun fossicker--widget-fname-notify (w &rest ignore)
  (let* ((fname (concat
                 (or (file-name-as-directory
                      (widget-value (fossicker--fget 'cntxt))) "")
                 (or (widget-value w) "")))
         (ext (file-name-extension fname nil))
         (types (fossicker--matching-types fname))
         (specs (fossicker--project-get 'spec))
         (type (fossicker--matching-spec types (mapcar 'car specs)))
         (spec (cdr (assq type specs)))
         (extl (fossicker--get-extension-list type ext))
         (path (fossicker--compile-path spec))
         prospect)
    (widget-value-set (fossicker--fget 'type) type)
    (if types
        (if type
            (progn
              (setq fossicker--fextl extl)
              (fossicker--message "SUCCESS:\n Found match for filename %S." fname)
              (widget-apply (fossicker--fget 'gen) :activate)
              (setq prospect (fossicker--prospect
                              (fossicker--generate-vein-map fname type)
                              (file-name-as-directory fossicker-data-path)
                              (fossicker--add-case-variations extl)))
              (when prospect
                (fossicker--message "SUCCESS:\n Found match for filename %S.\n Prospect: %s" fname prospect)
                (widget-value-set (fossicker--fget 'source)
                                  (fossicker--ffmt-src prospect))))
          (progn
            (widget-apply (fossicker--fget 'gen) :deactivate)
            (fossicker--message "WARNING:\n No matching type is included in project.\n Possible types: %S" types)))   
      (progn
        (widget-apply (fossicker--fget 'gen) :deactivate)
        (fossicker--message "WARNING:\n Couldn't match file name %S to regexp list of\n any fossicker type." fname)))
    
    
    (widget-setup)))

(defun fossicker--widget-fname-action (w &rest ignore)
  (let* ((fname (widget-value w))
         (fl (file-name-nondirectory fname))
         (cn (file-name-directory fname)))
    (when cn
      (widget-value-set (fossicker--fget 'cntxt) cn))
    (widget-value-set w fl)
    (widget-setup)))

(defun fossicker--widget-source-notify (w &rest ignore)
  (let ((src (read-file-name "Source: " "~/dev" nil t)))
    (if (file-regular-p src)
        (if (or (null fossicker--fextl) (string-match (concat "\\." (regexp-opt fossicker--fextl) "\\'") src))
            (progn
              (setq fossicker--fsrc src)
              (widget-value-set w (fossicker--ffmt-src src))
              (fossicker--message "SUCCESS:\n Successfully set the source to %S" src))
          (fossicker--message
           "ERROR:\n Source expected to be one of following formats: %S.\n Got %S." fossicker--fextl (file-name-extension src)))
      (fossicker--message "ERROR:\n Source %S is not a regular file." src))))

(defun fossicker--widget-generate-notify (&rest ignore)
  (fossicker--report
   (let* ((fname (file-name-nondirectory
                  (widget-value (fossicker--fget 'fname))))
          (context (or (file-name-directory
                        (widget-value (fossicker--fget 'fname)))
                       (widget-value (fossicker--fget 'cntxt))))
          (ext (file-name-extension fname nil))
          (specs (fossicker--project-get 'spec))
          (type (widget-value (fossicker--fget 'type)))
          (spec (cdr (assq type specs)))
          (fn (elt (assoc type fossicker--type-registry) 2))
          (path (fossicker--compile-path spec)))
     (fossicker--report
      (funcall fn path context fname ext (cdr spec) fossicker--fsrc)))))

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
          (cons nil (mapcar 'car (fossicker--get-types))))))


;;;; Widget Definition

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
                                  (widget-apply (fossicker--fget 'cntxt) :activate)
                                  (widget-apply (fossicker--fget 'source) :activate))
                              (progn
                                (fossicker-unset-project)
                                (widget-apply (fossicker--fget 'fname) :deactivate)
                                  (widget-apply (fossicker--fget 'cntxt) :activate)
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
   'cntxt
   (widget-create 'editable-field
                  :size 20
                  :tag (fossicker--ftag "Context")
                  :format "%t: %v\n"
                  "ui/buttons/"))
  (widget-apply (fossicker--fget 'cntxt) :deactivate)

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


;;; Fossicker Widget Provide

(provide 'fossicker-widget)

;;; fossicker-widget.el ends here
