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

;;;;;;;;;;
;;; Config
;;
;;

(defvar *config-path* nil
  "Fossicker configuration file path.")

(defvar *config* nil
  "Fossicker configuration plist.")

(defclass config-system (asdf:package-inferred-system)
  ((projects :initarg :projects)
   (legend :initarg :legend)))

;;
;;;; Load Libraries
;;
;;

;; (defun load-library (name)
;;   "Given a plugin, NAME, compile and load it."
;;   (let ((file (merge-pathnames-as-file
;;                fossicker-conf:*basedir*
;;                (format nil "lib/~(~A~)" name))))
;;     (multiple-value-bind (output-file error)
;;         (ignore-errors (compile-file file :verbose nil :print nil))
;;       (when error
;;         (warn "Error while compiling library ~A: ~A.~%" name error))
;;       (load (or output-file file) :verbose t))))

;; (defun load-libs (&rest libraries)
;;   "If supplied, load LIBS, else load libs supplied in LIBS variable."
;;   (let ((libs (or libraries (getf *config* :libs))))
;;     (when libs
;;       (dolist (lib libs (message "Fossicker libraries loaded: ~a.~%" libs))
;;         (load-library lib)))))

;;
;;;; Load Config
;;
;;

;; (defun get-config-path (config-path)
;;   "Check the supplied CONFIG-PATH and if one doesn't exist,
;; use the .fossickerrc in user home or lastly in repo."
;;   (let ((home-config (merge-pathnames-as-file
;;                       (user-homedir-pathname) ".fossickerrc"))
;;         (repo-config (merge-pathnames-as-file
;;                       fossicker-conf:*basedir* ".fossickerrc")))
;;     (if config-path
;;         (if (not (directory-exists-p (pathname config-path)))
;;             (progn
;;               (message "Config file save location set to ~a.~%" config-path)
;;               (setf *config-path* (pathname-as-file config-path)))
;;             (progn
;;               (message "~a is a directory, using ~a as config save location instead.~%"
;;                        config-path
;;                        home-config)
;;               (setf *config-path* home-config)))
;;         (progn
;;           (message "Config file save location set to ~a~%" home-config)
;;           (setf *config-path* home-config)))
;;     (cond ((and config-path (file-exists-p (pathname config-path)))
;;            (pathname config-path))
;;           ((file-exists-p home-config) home-config)
;;           (t repo-config))))

;; (defun load-config (&optional config-path)
;;   "Find and load the fossicker configuration."
;;   (with-open-file (in (get-config-path config-path) :external-format :utf-8)
;;     (setf *config* (read in)))
;;   (setf fossicker-conf:*basedir*
;;         (or (getf *config* :base-path)
;;             fossicker-conf:*basedir*))
;;   (load-libs)
;;   (load-projects)
;;   (set-project (getf *config* :default)))
