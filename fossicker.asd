;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ASDF-USER -*-
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
			   #:net.didierverna.clon
               #:qtools
               #:qtcore
               #:qtgui)
  :serial t
  :components ((:module core
                :components
                ((:file "package")
                 (:file "file")
                 (:file "documentation")
                 (:file "interaction")
                 (:file "exposure")
                 (:file "asset")
                 (:file "project")
                 (:file "config")
                 (:file "interface")
                 (:file "fossicker")))
               (:module interfaces
                :components
                ((:file "cli")
                 (:file "srv")
                 (:file "gui"))))
  ;; :perform (load-op :after (o s)
  ;;                   (funcall (find-symbol "INITIALIZE" "FOSSICKER")))
  )

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
