;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

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
  (:import-from :ironclad
                #:byte-array-to-hex-string
                #:digest-file)
  
  (:export                          ; file.lisp
   ;;; FILE
   #:*default-digest-name*
   #:calculate-hash                 ; (digest pathname)
   ;;;;;;;;;;;;;;;;;;;;;
   #:checksum ;;;;;;;;;;
   #:make-checksum                  ; (pathname &optional digest)
   #:checksum-digest                ; R(structure)
   #:checksum-hash                  ; R(structure)
   ;;;;;;;;;;;;;;;;;;;;;
   #:checksum-equal                 ; (checksum1 checksum2)
   #:intention
   #:file-status
   #:file-pathname
   ;;;;;;;;;;;;;;;;;
   #:file ;;;;;;;;;;
   #:make-file                      ; (namestring &key status &allow-other-keys)
   #:file-pathname                  ; R(structure)
   #:file-status                    ; A(structure)
   ;;;;;;;;;;;;;;;;;
   #:file-compute-checksum          ; (file &rest args)
   #:file-store-checksum            ; (file &rest args)
   #:file-verify                    ; (file)
   #:file-report-status             ; (file)
   #:file-sanity-checks             ; (file)
   #:file-confirm-intention         ; (file)
   #:file-safely-remove             ; (file)
   )

  (:export                          ; documentation.lisp
   ;;; DOCUMENTATION EXTENSIONS
   ;;;; DOCUMENTATION*
   #:documentation*                 ; (x doc-type)
   )

  (:export                          ; exposure.lisp
   ;;; INTERACTIVE
   ;;;; INTERACTIVE OBJECT & CLASS
   #:interactive-object
   #:interactive-class
   ;;;; INTERACTIVE SLOT DEFINITIONS
   #:slot-definition-interactivep   ; (slot)
   #:interactive-direct-slot-definition
   #:interactive-effective-slot-definition
   #:*interactive-effective-slot-definition-class*
   ;;;; INTERACTIVE SPECIAL LAYERED ACCESS CLASS
   #:interactive-special-layered-access-class
   ;;;; INTERACTIVE SPECIAL LAYERED SLOT DEFINITIONS
   #:interactive-special-layered-direct-slot-definition
   #:interactive-effective-slot-definition-in-layers
   #:interactive-layered-effective-slot-definition
   #:*interactive-layered-effective-slot-definition-class*
   ;;;; EXPOSURE
   #:exposure
   ;;;; EXPOSING FUNCTIONS
   #:document-slot                  ; (slot)
   #:slot-or-null
   #:infer-presentation             ; (type)
   #:type-specifier
   #:documentation-or-null
   ;;;;;;;;;;;;;;;;;;;;
   #:initarg ;;;;;;;;;;
   #:make-initarg
   #:initarg-keyword                ; R(structure)
   #:initarg-type                   ; R(structure)
   #:initarg-initform               ; R(structure)
   #:initarg-presentation           ; R(structure)
   #:initarg-documentation          ; R(structure)
   #:initarg-value                  ; (structure)
   ;;;;;;;;;;;;;;;;;;;;
   #:infer-initarg-properties       ; (class &rest initargs)
   #:compute-initarg-properties     ; (class)
   )
  
  (:export                          ; asset.lisp
   ;;; ASSETS
   ;;;; DISPATCHER
   #:some-regex                     ; (namestring &rest regex)
   #:dispatch                       ; (namestring)
   ;;;; RESOURCE
   #:resource
   ;;;; ASSET
   ;;;;;;;;;;;;;;;;;;
   #:asset ;;;;;;;;;;
   #:asset-formats                  ; A(object)
   ;;asset-path
   #:asset-namestring               ; A(object)
   #:asset-source                   ; A(object)
   #:asset-files                    ; A(object)
   #:asset-date                     ; R(object)
   ;;asset-description
   ;;asset-benchmark
   ;;;;;;;;;;;;;;;;;;          
   ;;;; PROSPECTABLE FORMAT RESTRICTIONS
   #:restrict-prospectable-formats  ; (asset)
   #:prospect-any
   #:prospect-same
   #:prospect-custom
   ;;;; ASSET GENERICS
   ;; #:save                        ; (asset)
   ;; #:browse                      ; (asset)
   ;;;; Prospect
   #:map-to-veins                   ; (namestring legend)
   #:generate-vein-map              ; (asset legend)
   #:prospect                       ; (map dir formats &optional prospect)
   #:prospect-asset                 ; (asset legend mine)
   ;; #:compile-path                ; (spec)
   ;; #:report                      ; (result)
   )

  (:export                          ; project.lisp
   ;;; PROJECTS
   #:*project-registry*
   #:*project*
   #:pathname-designator
   ;;;;;;;;;;;;;;;;;;;;
   #:project ;;;;;;;;;;
   #:project-file                   ; R(object)
   #:project-name                   ; R(object)
   #:project-root                   ; R(object)
   #:project-path                   ; R(object)
   #:project-specs                  ; R(object)
   #:project-legend                 ; R(object)
   #:project-mine                   ; A(object)
   #:project-draft                  ; A(object)
   #:project-selected               ; A(object)
   #:project-assets                 ; A(object)
   #:project-log                    ; R(object)
   ;;;;;;;;;;;;;;;;;;;;
   #:get-data-from-file             ; (path)
   #:infer-project-root             ; (pathname)
   #:infer-project-name             ; (pathname)
   #:infer-project-class            ; (pathname)
   #:matching-spec                  ; (dispatch specs)
   #:infer-asset-initargs           ; (namestring project initargs)
   #:initialize-draft               ; (draft class initargs)
   #:compile-draft-closure          ; (draft class initargs)
   #:draft                          ; (project namestring)
   #:accept                         ; (project interactive)
   #:redraft                        ; (project &key clean)
   #:select                         ; (project &optional index)
   #:generate                       ; (project)
   ;;;; SELECTION
   #:project-root-p                 ; (project-root)
   #:find-project                   ; (projects)
   #:get-project                    ; (name)
   #:set-project                    ; (&optional name)
   #:clear-project-registry         ; ()
   #:load-project                   ; (&rest initargs &key file (import t) &allow-other-keys)
   #:unload-project                 ; (name)
   )
  
  (:export                          ; config.lisp
   ;;; CONFIG
   #:*default-config-system*
   #:*config*
   #:*repository*
   #:*default-mine-directory*
   ;;;;;;;;;;;;;;;;;;;;;;;;;;
   #:configuration ;;;;;;;;;;
   #:startup-notice                 ; R(configuration)
   #:projects                       ; A(configuration)
   #:default-project                ; A(configuration)
   #:legend                         ; A(configuration)
   #:mine                           ; A(configuration)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;
   #:configure                      ; (&key (system *default-config-system*) force)
   ;;;; Project Operations
   #:load-projects                  ; (config)
   #:add-project                    ; (config file &optional root)
   #:remove-project                 ; (config file)
   )
  
  (:export                          ; interface.lisp
   ;;; INTERFACE
   #:message                        ; (&rest args)
   #:prompt                         ; (&rest args)
   )

  (:export                          ; fossicker.lisp
   ;;; FOSSICKER
   ;;;; STARTUP
   #:*startup-notice*
   #:initialize                     ; ()
   ))

(in-package :fossicker)

(defpackage :fossicker-user
  (:use :cl :fossicker))
