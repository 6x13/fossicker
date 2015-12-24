;;; fossicker-project.el --- Project editing widget for Fossicker.

;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 23 December 2015
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
(require 'fossicker-widget)
(require 'cl-extra)

(eval-when-compile
  (require 'wid-edit))
(require 'widget)
(require 'cus-edit)
(require 'info)

(defvar-local fossicker--edited-path nil
  "Local variable holding the path of currently edited project.")

(defvar-local fossicker--edited-project nil
  "Local variable holding the value of currently edited project.")

(defun fossicker--list-type-widgets ()
  "List of type widgets to choose."
  (let (value)
    (dolist (element
             (fossicker--get-types)
             value)
      (let* ((atsym (elt element 0))
             (atwidget (elt element 4))
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

(defun fossicker--project-generate-notify (&rest ignore)
  (let ((data fossicker--edited-project) ;; So temp-buffer can see data.
        (path fossicker--edited-path))
    (if (and data (y-or-n-p (format "Write project data to %S?" path)))
        (write-region
         (with-temp-buffer
           (cl-prettyprint data)
           (buffer-string))
         nil path nil))))

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
                                         :tag "Project Root "
                                         :value "~/")
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
                                                '(fossicker--type-undefined-widget)))))))
  
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


;;; Fossicker Project Provide

(provide 'fossicker-project)

;;; fossicker-project.el ends here
