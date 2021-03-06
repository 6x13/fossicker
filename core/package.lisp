;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 23 October 2015

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

(in-package :cl-user)

(defpackage :fossicker
  (:use :contextl-common-lisp)
  (:import-from :asdf
                #:clear-system
                #:load-system
                #:require-system
                #:find-system
                #:package-inferred-system
                #:system-source-directory)
  (:import-from :uiop/stream
                #:with-safe-io-syntax)
  (:import-from :uiop/filesystem
                #:file-exists-p
                #:directory-exists-p
                #:directory-files
                #:delete-file-if-exists)
  (:import-from :uiop/pathname
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
  (:import-from :ironclad
                #:byte-array-to-hex-string
                #:digest-file)
  ;; FILE
  (:intern                          ; file.lisp
   #:*default-digest-name*
   #:calculate-hash                 ; (digest pathname)
   #:checksum ;;;;;;;;;
   #:make-checksum   ;;             ; (pathname &optional digest)
   #:checksum-p      ;;             ; (x)
   #:checksum-digest ;;             ; (structure)
   #:checksum-hash   ;;             ; (structure)
   ;;;;;;;;;;;;;;;;;;;;
   #:checksum-equal                 ; (checksum1 checksum2)
   #:intention
   #:file-compute-checksum          ; (file &rest args)
   #:file-sanity-checks             ; (file)
   )
  (:export                          ; file.lisp
   #:file ;;;;;;;;;;;
   #:make-file     ;;               ; (namestring &key status
   ;;              ;;                   ; &allow-other-keys)
   #:file-pathname ;;               ; (structure)
   #:file-status   ;;               ; (structure)
   ;;;;;;;;;;;;;;;;;;
   #:file-store-checksum            ; (file &rest args)
   #:file-verify                    ; (file)
   #:file-report-status             ; (file)
   #:file-confirm-intention         ; (file)
   #:file-safely-remove             ; (file)
   )
  ;; DOCUMENTATION
  (:export                          ; documentation.lisp
   #:documentation*                 ; (x doc-type)
   )
  ;; EXPOSURE
  (:intern                          ; exposure.lisp
   #:interactive-direct-slot-definition
   #:interactive-effective-slot-definition
   #:*interactive-effective-slot-definition-class*
   #:interactive-special-layered-access-class
   #:interactive-special-layered-direct-slot-definition
   #:interactive-effective-slot-definition-in-layers
   #:interactive-layered-effective-slot-definition
   #:*interactive-layered-effective-slot-definition-class*
   #:slot-or-null
   #:infer-presentation             ; (type)
   #:documentation-or-null
   )
  (:export                          ; exposure.lisp
   #:interactive-object
   #:interactive-class
   #:slot-definition-interactivep   ; (slot)
   #:exposure
   #:document-slot                  ; (slot)
   #:type-specifier
   #:infer-initarg-properties       ; (class &rest initargs)
   #:compute-initarg-properties     ; (class)
   #:initarg ;;;;;;;;;;;;;;;;
   #:make-initarg          ;;
   #:initarg-p             ;;       ; (x)
   #:initarg-keyword       ;;       ; (structure)
   #:initarg-type          ;;       ; (structure)
   #:initarg-initform      ;;       ; (structure)
   #:initarg-presentation  ;;       ; (structure)
   #:initarg-documentation ;;       ; (structure)
   #:initarg-value         ;;       ; (structure)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;
   )
  ;; ASSET
  (:intern                          ; asset.lisp
   #:map-to-veins                   ; (rqststring legend)
   #:generate-vein-map              ; (asset legend)
   #:prospect                       ; (map dir formats &optional prospect)
   )
  (:export                          ; asset.lisp
   #:some-regex                     ; (rqststring &rest regex)
   #:dispatch                       ; (rqststring)
   #:resource
   #:asset ;;;;;;;;;;;;;
   #:asset-formats    ;;            ; (object)
   #:asset-rqststring ;;            ; (object)
   #:asset-source     ;;            ; (object)
   #:asset-files      ;;            ; (object)
   #:asset-date       ;;            ; (object)
   ;;;;;;;;;;;;;;;;;;;;;
   #:restrict-prospectable-formats  ; (asset)
   #:prospect-any
   #:prospect-same
   #:prospect-custom
   ;;save                           ; (asset)
   ;;browse                         ; (asset)
   #:prospect-asset                 ; (asset legend mine)
   )
  ;; PROJECT
  (:intern                          ; project.lisp
   #:pathname-designator
   #:get-data-from-file             ; (path)
   #:infer-project-root             ; (pathname)
   #:infer-project-name             ; (pathname)
   #:infer-project-class            ; (pathname)
   #:matching-spec                  ; (dispatch specs)
   #:infer-asset-initargs           ; (rqststring project initargs)
   #:initialize-draft               ; (draft class initargs)
   #:compile-draft-closure          ; (draft class initargs)
   #:project-root-p                 ; (project-root)
   #:find-project                   ; (projects)
   )
  (:export                          ; project.lisp
   #:*project-registry*
   #:*project*
   #:project ;;;;;;;;;;;
   #:project-file     ;;            ; (object)
   #:project-name     ;;            ; (object)
   #:project-root     ;;            ; (object)
   #:project-path     ;;            ; (object)
   #:project-specs    ;;            ; (object)
   #:project-legend   ;;            ; (object)
   #:project-mine     ;;            ; (object)
   #:project-draft    ;;            ; (object)
   #:project-selected ;;            ; (object)
   #:project-assets   ;;            ; (object)
   #:project-log      ;;            ; (object)
   ;;;;;;;;;;;;;;;;;;;;;
   #:draft                          ; (project rqststring)
   #:submit                         ; (project interactive)
   #:redraft                        ; (project &key clean)
   #:select                         ; (project &optional index)
   #:generate                       ; (project)
   #:get-project                    ; (name)
   #:set-project                    ; (&optional name)
   #:clear-project-registry         ; ()
   #:load-project                   ; (&rest initargs
                                        ; &key file (import t)
                                        ; &allow-other-keys)
   #:unload-project                 ; (name)
   )
  ;; CONFIG
  (:export                          ; config.lisp
   #:*default-config-system*
   #:*config*
   #:*repository*
   #:*default-mine-directory*
   #:configuration ;;;;
   #:startup-notice  ;;             ; (configuration)
   #:projects        ;;             ; (configuration)
   #:default-project ;;             ; (configuration)
   #:legend          ;;             ; (configuration)
   #:mine            ;;             ; (configuration)
   ;;;;;;;;;;;;;;;;;;;;
   #:configure                      ; (&key (system *default-config-system*)
                                        ; force)
   #:load-projects                  ; (config)
   #:add-project                    ; (config file &optional root)
   #:remove-project                 ; (config file)
   )
  ;; INTERFACE
  (:export                          ; interface.lisp
   #:message                        ; (&rest args)
   #:prompt                         ; (&rest args)
   )
  ;; FOSSICKER
  (:export                          ; fossicker.lisp
   #:*startup-notice*
   #:initialize                     ; ()
   ))

(in-package :fossicker)

(defpackage :fossicker-user
  (:use :cl :fossicker))
