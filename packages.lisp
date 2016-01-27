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
  ;; (:import-from :cl-fad #:canonical-pathname)
  ;; (:import-from :cl-fad #:list-directory)
  ;; (:import-from :cl-fad #:file-exists-p)
  ;; (:import-from :cl-fad #:directory-exists-p)
  ;; (:import-from :cl-fad #:pathname-as-file)
  ;; (:import-from :cl-fad #:pathname-as-directory)
  ;; (:import-from :cl-fad #:merge-pathnames-as-file)
  ;; (:import-from :cl-fad #:merge-pathnames-as-directory)
  ;; (:import-from :cl-fad #:pathname-directory-pathname)
  ;; (:import-from :cl-ppcre #:create-scanner)
  ;; (:import-from :cl-ppcre #:scan)
  (:export #:*config*
           #:*repository*
           #:configuration
           #:configure
           ;; #:load-libs
           ;; #:load-config
           ;; #:register-type
           ;; #:*project*
           ;; #:get-project
           ;; #:set-project
           ;; #:load-project
           ;; #:unload-project
           ;; #:load-projects
           ;; #:add-project
           ;; #:remove-project
           ;; #:generate
           ))

;; (defpackage #:fossicker-widget
;;   (:use #:cl+qt
;;         #:fossicker)
;;   (:import-from :cl-fad #:merge-pathnames-as-file)
;;   (:export #:main))

;; (defpackage :fossicker-user
;;   (:nicknames :fcku)
;;   (:use :fossicker :fossicker-widget))