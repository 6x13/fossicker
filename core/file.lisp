;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 7 February 2016

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

;; CAUTION:  Operations below  have  destructive side-effects  on file  system.
;; Strict  type  checking  is  required  in  order  to  make  sure  we  do  not
;; accidentally operate on wrong entities. Optimize for safety to make sure all
;; type checks are done.
(declaim (optimize safety))

;;;;;;;;
;;; File
;;
;;

(declaim (type keyword *default-digest-name*))

(defvar *default-digest-name* :md5
  "Name of the digest algorithm to be used for checksums by default.")

(defun calculate-hash (digest pathname)
  "Calculates the CHECKSUM of file using DIGEST."
  (byte-array-to-hex-string
   (digest-file digest pathname)))

(defstruct (checksum (:type vector) :named
                     (:constructor make-checksum
                         (pathname
                          &optional digest
                          &aux (hash (calculate-hash digest pathname)))))
  "The CHECKSUM structure. First element  is DIGEST-NAME. The second element is
the  HASH calculated  for  the  physical file  located  at  PATHNAME using  the
optionally provided DIGEST-NAME."
  (digest *default-digest-name*
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
                 (:constructor make-file (namestring &key status
                                          &allow-other-keys)))
  "The FILE struct.  PATHNAME is a PATHNAME object pointing to the stored file.
STATUS is the CHECKSUM  of the generated file if it is  of type CHECKSUM.  User
has chosen to write the file if STATUS is T, omit writing otherwise."
  (pathname (merge-pathnames* namestring)
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
  (checksum-equal status
                  (file-compute-checksum file (checksum-digest status))))

(defun file-report-status (file &aux (status (file-status file))
                                  (checksump (checksum-p status))
                                  (pathname (file-pathname file))
                                  (existsp (file-exists-p pathname)))
  "Reports status  of physical  file with extra  assumptions about  it. Overall
meaning of return values are as follows:

Primary Value : 'Physical' status.
Second Value  : 'Operational' status.
Third Value   : 'Safety' status.

Possible PRIMARY values and corresponding other values are as follows:

:CHECKS
File :SAVED by  Fossicker, it still exists in file  system unmodified. :SAFE to
operate.

:MODIFIED
File :SAVED by Fossicker but later  modified outside of Fossicker. Proceed with
:CAUTION.

:DELETED
File  :SAVED by  Fossicker but  later removed  outside of  Fossicker. :SAFE  to
operate.

:EXISTS
File exists in  file system.  It is either :PENDING  or :DISCARDED by Fossicker
probably because user has chosen not to overwrite it. Proceed with :CAUTION.

:ABSENT
File doesn't exist in file system.   It might be :DISCARDED by Fossicker either
because  it was  generated  by a  DRY-RUN  or  it was  discarded  by user  then
deleted. It might as well be :PENDING. :SAFE to operate."
  (cond
    ((and checksump existsp (file-verify file))
     (values :checks
             :saved
             :safe))
    ((and checksump existsp)
     (values :modified
             :saved
             :caution))
    (checksump
     (values :deleted
             :saved
             :safe))
    ((and status existsp)
     (values :exists
             :pending
             :caution))
    (status
     (values :absent
             :pending
             :safe))
    (existsp
     (values :exists
             :discarded
             :caution))
    (t
     (values :absent
             :discarded
             :safe))))

(defun file-sanity-checks (file &aux (pathname (file-pathname file)))
  "Make  sure  the pathname  actually  represents  a  file both  logically  and
physically."
  (message "Doing sanity checks on file: ~A." pathname)
  (assert (file-pathname-p pathname) nil
          "The calculated pathname does not represent a file.")
  (assert (not (directory-exists-p pathname)) nil
          "Calculated pathname represents a directory. Not a file.")
  (message "Sanity checks successful."))

(defun file-confirm-intention (file &aux (status (file-status file))
                                      (pathname (file-pathname file)))
  "If  intention is  to 'discard'  the operation,  simply RETURN-FROM  function
doing  nothing.   Otherwise  check  if  file already  exists  in  file  system,
confirming overwrite operation if so."
  (check-type status intention)
  (message "Confirming intention on file: ~A." pathname)
  (file-sanity-checks file)
  (if (and status (eq (file-report-status file) :exists))
      (if (setf (file-status file)
                (prompt "File exists at location ~A.~%~A"
                        pathname
                        "Sure you want to overwrite?"))
          (message "Confirmed. File will be overwritten.")
          (message "Discarded."))
      (message "No confirmation necessary.")))

(defun file-safely-remove (file &aux (pathname (file-pathname file)))
  "Make sanity checks about pathname and the physical file it represents. Query
file status. Interactively handle file removal."
  (file-sanity-checks file)
  (message "Starting remove operation on file: ~A." pathname)
  (multiple-value-bind (physical operational safety) (file-report-status file)
    (declare (ignore operational))
    (let ((safe (eq safety :safe)))
      (if safe
          (message
           (case physical
             (:checks
              (delete-file-if-exists pathname)
              "File was created by Fossicker and is unmodified. ~
               Safely removed file.")
             (:deleted
              "The file that was created by Fossicker seems to be removed ~
               externally. No cleaning necessary.")
             (:absent
              "Currently no file exists at location. No cleaning necessary.")))
          (if (prompt
               "~A Are you sure you want to remove the file?"
               (case physical
                 (:modified
                  "File that was created by Fossicker is modified externally.")
                 (:exists
                  "A file already exists at the calculated location.")))
              (progn
                (message "Action confirmed. Removing file.")
                (delete-file-if-exists pathname)
                (message "File removed."))
              (message "Discarded. Skipping removal."))))))
