;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-

(defpackage #:fossicker-conf
  (:export #:*basedir* #:*config*))

#-asdf3.1 (error "Fossicker requires ASDF 3.1")
(defsystem #:fossicker
  :class :package-inferred-system
  :name "fossicker"
  :description "Open Source Asset Prospector"
  :version "0.1"
  :maintainer "Kenan Bölükbaşı <kenanbolukbasi@gmail.com>"
  :licence "GPL"
  :pathname "src/"
  :depends-on ((:version "asdf" "3.1.2")
               ;; #:cl-fad
               ;; #:cl-ppcre
               ;; #:qtools
               ;; #:qtcore
               ;; #:qtgui
               )
  :serial t
  :components ((:file "package")
               (:file "config")
               ;; (:file "types")
               ;; (:file "projects")
               ;; (:file "prospect")
               ;; (:file "cli")
               ;; (:file "widget")
               (:file "fossicker"))
  :perform (load-op :after (o s)
                    (require-system :fossicker-user)
                    (defparameter fossicker-conf:*basedir*
                      (make-pathname :name nil
                                     :type nil
                                     :defaults *load-truename*)
                      "Directory containing the Fossicker package. This is used
                      to  load the  supporting  fossicker  type libraries.  The
                      default value is automatically computed from the location
                      of the Fossicker package.")
                    (defparameter fossicker-conf:*config*
                      (find-system :fossicker-user))))

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
