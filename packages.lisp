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
                ;; STREAM
                #:with-safe-io-syntax
                ;; FILESYSTEM
                #:file-exists-p
                #:directory-exists-p
                #:directory-files
                #:delete-file-if-exists
                ;; PATHNAME
                #:file-pathname-p
                #:ensure-directory-pathname
                #:pathname-directory-pathname
                #:merge-pathnames*
                #:subpathname
                #:subpathname*
                #:unix-namestring
                #:parse-unix-namestring)
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

(defpackage #:fossicker-widget
  (:use #:cl+qt
        #:fossicker)
  (:import-from :uiop
                ;; PATHNAME
                #:subpathname*)
  (:export #:main))

(in-package :fossicker)

(defpackage :fossicker-user
  (:use :cl :fossicker))
