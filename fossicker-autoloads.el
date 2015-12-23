;;; fossicker-autoloads.el --- autoload definitions for Fossicker.

;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 16 December 2015
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

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.

;;; Code:


;;;### (autoloads nil "fossicker" "fossicker.el" (22139 7507 879905
;;;;;;  974000))
;;; Generated autoloads from fossicker.el

(let ((loads (get 'fossicker 'custom-loads))) (if (member '"fossicker" loads) nil (put 'fossicker 'custom-loads (cons '"fossicker" loads))))

(defvar fossicker-libs '(fossicker-all) "\
A list of lib packages to load with FOSSICKER.
Defaults to FOSSICKER-ALL meta-package.")

(autoload 'fossicker-load-libs "fossicker" "\
If supplied, load LIBS, else load libs supplied in FOSSICKER-LIBS variable.

\(fn &rest LIBS)" nil nil)

(autoload 'fossicker-register-type "fossicker" "\
Register a new fossicker type. 
Fossicker TYPE is determined according a :REGEXP,
usually matching file extensions.
The asset picked from fossicker data path is
processed by :FUNCTION in order to fit
the type specification.
:FORMATS defines which file format to pick
among possible matches in the data path.
You can add customizations parameeters
to type using :WIDGETS.

\(fn NAME OVERRIDE &rest ARGS)" nil nil)

(defvar fossicker-legend '(("_b_" "button") ("_n_" "normal") ("_p_" "pressed") ("_e_" "enabled")) "\
List of regular expressions and the directory names they map to.")

(custom-autoload 'fossicker-legend "fossicker" t)

(defvar fossicker-projects nil "\
The list of fossicker project paths.")

(custom-autoload 'fossicker-projects "fossicker" t)

(autoload 'fossicker-load-projects "fossicker" "\
Loads all projects in FOSSICKER-PROJECTS.

\(fn)" nil nil)

(autoload 'fossicker-customize "fossicker" "\
Customize fossicker group.

\(fn)" t nil)

(autoload 'fossicker-set-project "fossicker" "\
Manually select a project among fossicker projects list.

\(fn &optional PROJECT)" t nil)

(autoload 'fossicker-unset-project "fossicker" "\
Set fossicker-project to nil.

\(fn)" t nil)

(autoload 'fossicker-auto-select-project "fossicker" "\
Automatically select a project among fossicker projects list.
Checks the project root of each fossicker project against
the current buffer path to find the project buffer
belongs to.

\(fn)" t nil)

(autoload 'fossicker-generate "fossicker" "\
Generates the asset according to the double-quoted text
at current cursor position.

\(fn &optional FILENAME)" t nil)

(autoload 'fossicker-mode "fossicker" "\
Toggle Fossicker mode.

\(fn &optional ARG)" t nil)

(defvar global-fossicker-mode nil "\
Non-nil if Global-Fossicker mode is enabled.
See the command `global-fossicker-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-fossicker-mode'.")

(custom-autoload 'global-fossicker-mode "fossicker" nil)

(autoload 'global-fossicker-mode "fossicker" "\
Toggle Fossicker mode in all buffers.
With prefix ARG, enable Global-Fossicker mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Fossicker mode is enabled in all buffers where
`(lambda nil (fossicker-mode t))' would do it.
See `fossicker-mode' for more information on Fossicker mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "fossicker-widget" "fossicker-widget.el" (22139
;;;;;;  7366 460672 107000))
;;; Generated autoloads from fossicker-widget.el

(autoload 'fossicker "fossicker-widget" "\
Create the widgets for asset generation.

\(fn)" t nil)

;;;***


(provide 'fossicker-autoloads)

;;; fossicker-autoloads.el ends here
