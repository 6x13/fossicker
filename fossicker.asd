;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-

#-asdf3.1 (error "Fossicker requires ASDF 3.1")
(defsystem #:fossicker
  :class :package-inferred-system
  :name "fossicker"
  :description "Open Source Asset Prospector"
  :version "0.1"
  :maintainer "Kenan Bölükbaşı <kenanbolukbasi@gmail.com>"
  :licence "GPL"
  :depends-on ((:version #:asdf "3.1.2")
               #:uiop
               #:alexandria
               #:contextl
               #:ironclad
               #:cl-ppcre
               #:trivial-gray-streams
               #:qtools
               #:qtcore
               #:qtgui)
  :serial t
  :components ((:module core
                :components
                ((:file "package")
                 ;; (:file "persistence")
                 (:file "file")
                 (:file "documentation")
                 (:file "exposure")
                 (:file "asset")
                 (:file "project")
                 (:file "config")
                 ;; (:file "prospect")
                 (:file "interface")
                 (:file "fossicker")))
               (:module interfaces
                :components
                (;; (:file "cli")
                 ;; (:file "srv")
                 (:file "gui"))))
  :perform (load-op :after (o s)
                    (funcall (find-symbol "INITIALIZE" "FOSSICKER"))))

(register-system-packages "closer-mop"
                          '(:c2mop
                            :closer-common-lisp
                            :c2cl
                            :closer-common-lisp-user
                            :c2cl-user))

(register-system-packages "contextl"
                          '(:contextl
                            :contextl-common-lisp
                            :cxcl
                            :contextl-user
                            :cx-user))

(register-system-packages "fossicker"
                          '(:fossicker-user))
