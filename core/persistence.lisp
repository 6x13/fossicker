;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 4 February 2016

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

;; (declaim (optimize
;;           (speed 0) (compilation-speed 0) (safety 3) (debug 3)))

;;;;;;;;;;
;;; Reader
;;
;;

;;;;;;;;;
;;; Class
;;
;;

;;;;;;;;;;;;;;;;;;;
;;; Store & Restore
;;
;;

;; (defgeneric store (project)
;;   (:documentation
;;    "")
;;   (:method ((project project)))
;;   (store (merge-pathnames* (project-file-directory project) "") ()))

(defun get-history-path (project
						 &aux (file (project-file project)))
  (merge-pathnames* (format nil "~a.history.~a"
							(directory-namestring file)
							(file-namestring file))))

(defun save-project-history (&optional name &aux (project nil))
  "Save named project history, or if null, save active project history."
  (assert *project-registry* nil
          "No fossicker projects defined. You need at least one.")
  (assert (or (null name)
              (member name
                      *project-registry*
                      :key #'project-name
                      :test #'string=))
          nil "~a is not in project list." name)
  (setf project (if name (get-project name) *project*))
  (with-open-file (out (get-history-path project)
					   :direction :output
					   :if-exists :supersede
					   :if-does-not-exist :create)
	(loop for asset in (project-assets project)
		  doing (format out "~&~S" (marshal asset))))
  (message "History for the project ~a saved to disk." project))

(defun load-project-history (&optional name &aux (project nil))
  "Save named project history, or if null, save active project history."
  (assert *project-registry* nil
          "No fossicker projects defined. You need at least one.")
  (assert (or (null name)
              (member name
                      *project-registry*
                      :key #'project-name
                      :test #'string=))
          nil "~a is not in project list." name)
  (setf project (if name (get-project name) *project*))
  (with-open-file (in (get-history-path project)
					  :direction :input)
	(loop with eof = (gensym)
		  for object = (read in nil eof)
		  until (eq object eof)
		  collecting object into assets
		  finally (setf (project-assets project) assets)))
  (message "History for the project ~a loaded from disk." project))
