;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(in-package :cl-user)

(defpackage :fossicker
  (:use :contextl-common-lisp)
  (:import-from :asdf
                #:clear-system
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
           #:initialize
           #:load-projects
           #:add-project
           #:remove-project
           #:*project*
           #:*project-registry*
           #:get-project
           #:set-project
           #:load-project
           #:unload-project
           #:generate
           #:dispatch
           #:save))

;; (defpackage #:fossicker-widget
;;   (:use #:cl+qt
;;         #:fossicker)
;;   (:import-from :cl-fad #:merge-pathnames-as-file)
;;   (:export #:main))

(in-package :fossicker)

(defpackage :fossicker-user
  (:use :cl :fossicker))
