;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(in-package :cl-user)
(defpackage #:fossicker
  (:use #:cl+qt)
  (:import-from :cl-fad #:canonical-pathname)
  (:import-from :cl-fad #:list-directory)
  (:import-from :cl-fad #:file-exists-p)
  (:import-from :cl-fad #:directory-exists-p)
  (:import-from :cl-fad #:pathname-as-file)
  (:import-from :cl-fad #:pathname-as-directory)
  (:import-from :cl-fad #:merge-pathnames-as-file)
  (:import-from :cl-fad #:merge-pathnames-as-directory)
  (:import-from :cl-fad #:pathname-directory-pathname)
  (:import-from :cl-ppcre #:create-scanner)
  (:import-from :cl-ppcre #:scan)
  (:export #:*config*
           #:load-libs
           #:load-config
           #:register-type
           #:*project*
           #:add-project
           #:remove-project
           #:load-projects
           #:get-project
           #:show-current-project
           #:set-project
           #:unset-project
           #:auto-select-project
           #:generate
           #:main))

