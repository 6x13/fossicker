;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 7 February 2016

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

;;;;;;;;
;;; File
;;
;;

(defun calculate-hash (digest pathname)
  "Calculates the CHECKSUM of file using DIGEST."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file digest pathname)))

(defstruct (checksum (:type vector) :named
                     (:constructor make-checksum
                         (pathname
                          &optional digest
                          &aux (hash (calculate-hash digest pathname)))))
  "The CHECKSUM structure. First element  is DIGEST-NAME. The second element is
the  HASH calculated  for  the  physical file  located  at  PATHNAME using  the
optionally provided DIGEST-NAME."
  (digest :md5
   :type keyword
   :read-only t)
  (hash (error "No hash calculated.")
   :type string
   :read-only t))

(deftype checksum () '(satisfies checksum-p))

(defun checksum-equal (checksum1 checksum2
                       &aux
                         (digest1 (checksum-digest checksum1))
                         (digest2 (checksum-digest checksum2)))
  "Compares to CHECKSUMs that are generated with the same digest algorithm."
  (assert (eq digest1 digest2) nil
          "~A~%Trying to compare ~A with ~A!"
          "The digest specifications for provided checksums do not match."
          digest1 digest2)
  (string-equal (checksum-hash checksum1)
                (checksum-hash checksum2)))

(defstruct (file (:type vector) :named)
  "The  FILE  struct.   PATH  is  a PATHNAME  object  pointing  to  the  stored
file.  STATUS  is  the  CHECKSUM  of  the generated  file  if  it  is  of  type
CHECKSUM.  User has  chosen to  write the  file if  STATUS is  T, omit  writing
otherwise."
  (pathname (error "No PATH specified for FILE.")
   :type pathname
   :read-only t)
  (status t
   :type (or boolean checksum)))

(deftype file () '(satisfies file-p))

(defun file-hash (file &rest args)
  "Calculates the CHECKSUM of FILE."
  (apply #'make-checksum (file-pathname file) args))

(defun set-file-hash (file &rest args)
  "Calculates the CHECKSUM of FILE and saves it to FILE's FILE-STATUS slot."
  (setf (file-status file) (apply #'file-hash file args)))

(defun file-verify (file &aux (status (file-status file)))
  "Verifies the  FILE comparing  saved CHECKSUM to  the calculated  CHECKSUM of
physical file."
  (check-type status checksum)
  (checksum-equal status (file-hash file (checksum-digest status))))

;; (defmethod initialize-instance :after ((instance file) &key)
;;   (when (or (not (file-exists-p (path instance)))
;;             (prompt "File exists at location ~A. Sure you want to overwrite?"
;;                     (namestring (path instance))))
;;     ;; TODO: Call function to generate file.
;;     (setf (checksum instance) (md5sum instance))))

