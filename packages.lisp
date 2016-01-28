;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(in-package :cl-user)

(defpackage :fossicker
  (:nicknames :fck)
  (:use :cl)
  (:import-from :asdf
                #:load-system
                #:require-system
                #:find-system
                #:system-source-directory)
  (:import-from :uiop
                #:file-exists-p
                #:pathname-directory-pathname
                #:directory-exists-p
                #:directory-files)
  (:import-from :cl-ppcre
                #:create-scanner
                #:scan)
  (:export #:*config*
           #:*repository*
           #:configuration
           #:configure
           #:load-projects
           #:add-project
           #:delete-project
           #:*project*
           #:*project-registry*
           #:get-project
           #:set-project
           #:load-project
           #:unload-project
           #:generate))

;; (defpackage #:fossicker-widget
;;   (:use #:cl+qt
;;         #:fossicker)
;;   (:import-from :cl-fad #:merge-pathnames-as-file)
;;   (:export #:main))

(defpackage :fossicker-user
  (:nicknames :fcku)
  (:use :cl :fossicker))
