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

;;;;;;;;;;;;;
;;; Fossicker
;;
;;
;;;; Startup
;;

(defvar *startup-notice*
  "Fossicker  Copyright (C) 2015  Kenan Bölükbaşı
This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
This is  free software, and  you are welcome  to redistribute it  under certain
conditions; type `show c' for details.")

(defun initialize ()
  (configure)
  (when (startup-notice *config*) (message *startup-notice*)))
