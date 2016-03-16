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
  (:export                          ; exposure.lisp
   #:interactive-object
   #:interactive-class
   #:slot-definition-interactivep   ; (slot)
   #:exposure
   #:document-slot                  ; (slot)
   #:type-specifier
   #:infer-initarg-properties       ; (class &rest initargs)
   #:compute-initarg-properties     ; (class)
   )
  ;; ASSET
  (:intern                          ; asset.lisp
   #:map-to-veins                   ; (namestring legend)
   #:generate-vein-map              ; (asset legend)
   #:prospect                       ; (map dir formats &optional prospect)
   ;;compile-path                   ; (spec)
   ;;report                         ; (result)
   )
  (:export                          ; asset.lisp
   #:some-regex                     ; (namestring &rest regex)
   #:dispatch                       ; (namestring)
   #:resource
   #:asset ;;;;;;;;;;;;;
   #:asset-formats    ;;            ; (object)
   #:asset-namestring ;;            ; (object)
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
   #:infer-asset-initargs           ; (namestring project initargs)
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
   #:draft                          ; (project namestring)
   #:accept                         ; (project interactive)
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
