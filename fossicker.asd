;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(in-package #:cl-user)

(asdf:defsystem #:fossicker
  :name "fossicker"
  :description "Open Source Asset Prospector"
  :version "0.1"
  :maintainer "Kenan Bölükbaşı <kenanbolukbasi@gmail.com>"
  :licence "GPL"
  :pathname "src/"
  :depends-on (#:cl-fad
               #:cl-ppcre
               #:qtools
               #:qtcore
               #:qtgui)
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "types")
               (:file "projects")
               (:file "prospect")
               (:file "cli")
               (:file "widget")
               (:file "fossicker")))

(defpackage #:fossicker-conf (:export #:*basedir*))
(defparameter fossicker-conf:*basedir*
  (make-pathname :name nil :type nil :defaults *load-truename*)
    "Directory containing the Fossicker package.
This is used to load the supporting fossicker type libraries.
The default value is automatically computed from the location of
the Fossicker package.")
