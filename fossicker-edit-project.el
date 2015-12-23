;;; fossicker-edit-project.el --- Project editing widget for Fossicker.

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

(eval-when-compile
  (require 'wid-edit))
(require 'widget)
(require 'cus-edit)
(require 'info)

(defvar-local fossicker--edited-project nil
  "Local variable holding the value of currently edited project.")

(defun fossicker-edit-project ()
   "Create the widgets for asset generation."
  (interactive)
  (switch-to-buffer "*FOSSICKER-EDIT-PROJECT*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (fossicker-load-libs)
  
  (widget-create 'fossicker--project-widget
                 :notify (lambda (w &rest ignore)
                           (setq fossicker--edited-project (widget-value w)))
                 :value '("test"
                          (root . "~/dev/test/")
                          (path . "Resources/")
                          (spec . ((texture "textures/"
                                            (("ldpi/" . 32)
                                             ("mdpi/" . 64)
                                             ("hdpi/" . 128)
                                             ("retina/" . 256)))
                                   (model "models/")
                                   (bla "meh")
                                   (shader "shaders/")))))
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :tag "GENERATE"
                 :format "%[            %t            %]"
                 :button-face 'custom-button
                 :notify (lambda (&rest ignore)
                           (message "%s" fossicker--edited-project)))
  (use-local-map widget-keymap)
  (widget-setup))


;;; Fossicker Edit Project Provide

(provide 'fossicker-edit-project)

;;; fossicker-edit-project.el ends here
