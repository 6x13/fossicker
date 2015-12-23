;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(in-package #:cl-user)

(asdf:defsystem #:fossicker
  :name "fossicker"
  :description "Open Source Asset Prospector"
  :version "0.1"
  :maintainer "Kenan Bölükbaşı <kenanbolukbasi@gmail.com>"
  :licence "GPL"
  :serial t
  :depends-on (#:qtools #:qtcore #:qtgui)
  :components ((:file "package")
               (:file "fossicker")
               (:file "widget")))
