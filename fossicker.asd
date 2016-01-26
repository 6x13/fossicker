;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-

#-asdf3.1 (error "Fossicker requires ASDF 3.1")
(defsystem #:fossicker
  :class :package-inferred-system
  :name "fossicker"
  :description "Open Source Asset Prospector"
  :version "0.1"
  :maintainer "Kenan Bölükbaşı <kenanbolukbasi@gmail.com>"
  :licence "GPL"
  :depends-on ((:version "asdf" "3.1.2")
               ;; #:cl-fad
               ;; #:cl-ppcre
               ;; #:qtools
               ;; #:qtcore
               ;; #:qtgui
               )
  :serial t
  :components ((:module core
                :components
                ((:file "package")
                 (:file "config")
                 ;; (:file "types")
                 ;; (:file "projects")
                 ;; (:file "prospect")
                 (:file "fossicker")))
               (:module interface
                :components
                (
                 ;; (:file "cli")
                 ;; (:file "widget")
                 )))
  :perform (load-op :after (o s)
                    (let* ((package "FOSSICKER-CONFIGURATION")
                           (system '#:fossicker-user)
                           (config (find-symbol "*CONFIG*" package))
                           (repo (find-symbol "*REPOSITORY*" package)))
                      (require-system system)
                      (set config (find-system system))
                      (set repo (system-source-directory '#:fossicker)))))

(register-system-packages "closer-mop"
                          '(:c2mop
                            :closer-common-lisp
                            :c2cl
                            :closer-common-lisp-user
                            :c2cl-user))

(register-system-packages "fossicker"
                          '(:fck))

(register-system-packages "fossicker-user"
                          '(:fcku))
