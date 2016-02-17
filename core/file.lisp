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

(deftype checksum ()
  "Type definition for CHECKSUM structure."
  '(satisfies checksum-p))

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

(deftype intention ()
  "Type definition for BOOLEAN values to  use as file write operation intention
  in STATUS  slot of FILE structure.  T represents intent to  'write' while NIL
  represents intent to 'discard' the file."
  'boolean)

(deftype file-status ()
  "Type definition for STATUS slot of FILE structure."
  '(or intention checksum))

(deftype file-pathname ()
  "Type definition  for PATHNAME slot  of FILE structure. Ensures  the pathname
  represents a file."
  '(and pathname (satisfies file-pathname-p)))

(defstruct (file (:type vector) :named
                 (:constructor make-file (pathname &optional status)))
  "The FILE struct.  PATHNAME is a PATHNAME object pointing to the stored file.
STATUS is the CHECKSUM  of the generated file if it is  of type CHECKSUM.  User
has chosen to write the file if STATUS is T, omit writing otherwise."
  (pathname (error "No PATHNAME specified for FILE.")
   :type file-pathname
   :read-only t)
  (status t
   :type file-status))

(deftype file ()
  "Type definition for FILE structure."
  '(satisfies file-p))

(defun file-compute-checksum (file &rest args)
  "Calculates the CHECKSUM of FILE."
  (apply #'make-checksum (file-pathname file) args))

(defun file-store-checksum (file &rest args)
  "Calculates the CHECKSUM of FILE and saves it to FILE's FILE-STATUS slot."
  (setf (file-status file) (apply #'file-compute-checksum file args)))

(defun file-verify (file &aux (status (file-status file)))
  "Verifies the  FILE comparing  saved CHECKSUM to  the calculated  CHECKSUM of
physical file."
  (check-type status checksum)
  (checksum-equal status (file-compute-checksum file (checksum-digest status))))

(defun file-confirm-intention (file &aux (status (file-status file)))
  "If file operation intention is  'discard', simply RETURN-FROM function doing
nothing.  Otherwise  check if  file already exists  in file  system, confirming
overwrite operation if so."
  (check-type status intention)
  (unless status (return-from file-confirm-intention)) ; Short circuit.
  (unless (or (not (file-exists-p (file-pathname file)))
              (prompt "File exists at location ~A.~%~A"
                      (file-pathname file)
                      "Sure you want to overwrite?"))
    (setf (file-status file) nil)))

;; (defun file-confirm-removal (file &aux (status (file-status file)))
;;   "If file to be removed exists in file system, check if it matches the recorded "
;;   (unless status (return-from file-confirm)) ; Short circuit.
;;   (when (or (not (file-exists-p (path instance)))
;;             (prompt "File exists at location ~A.~%~A"
;;                     (file-pathname file)
;;                     "Sure you want to overwrite?"))))
