;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 20 October 2015

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

(asdf:register-system-packages "qtools"
							   '(:cl+qt))

(defpackage #:fossicker-ui-qt
  (:use #:cl+qt
        #:fossicker
        #:trivial-gray-streams)
  (:import-from :uiop
                ;; PATHNAME
                #:subpathname*)
  (:import-from :cl-ppcre
                #:regex-replace-all)
  (:export #:main))

(in-package #:fossicker-ui-qt)
(in-readtable :qtools)

;;;;;;;;;;;;;;;;;;;;
;;; Fossicker Widget
;;
;;
;;;; Fontify & Output
;;
;;

(defun output (log format-string &rest args)
  "Formats and outputs to LOG widget according to given arguments."
  (q+:move-cursor log (q+:qtextcursor.end))
  (q+:insert-html log (apply #'format NIL format-string args))
  (q+:move-cursor log (q+:qtextcursor.end)))

(defun escape (text)
  "Escapes TEXT that might be confused with HTML tags."
  (flet ((r (text find replace)
           (regex-replace-all find text replace)))
    (r (r (r text "&" "&amp;") "<" "&lt;") ">" "&gt;")))

(defun fontify (color format-string &rest args)
  "Applies formatting and fontification to  text with specified style for given
arguments for printing  to log widget.  Currently only COLOR  is supported as a
style."
  (format nil "<span style=\"color:~a;\">~a</span>"
          (symbol-name color)
          (apply #'format NIL format-string args)))

(defun fontify-error (error)
  "Formats and fontifies ERROR for log widget."
  (format nil "~a~a<br />"
          (fontify :red "Error: ")
          (fontify :grey "~a<br />[Condition of type ~a]"
                   (escape (princ-to-string error))
                   (escape (princ-to-string (type-of error))))))

;;
;;;; Stream
;;
;;

(defclass log-stream (fundamental-character-output-stream
                      trivial-gray-stream-mixin)
  ((log
    :initarg :log
    :initform (error "LOG required.")
    :accessor log-stream-log
    :documentation "Log widget that stream will output.")
   (buffer
    :initform (make-string-output-stream)
    :accessor buffer
    :documentation "Stream buffer."))
  (:documentation "The stream to use for logging in widget."))

(defmethod stream-clear-output ((stream log-stream))
  (setf (buffer stream) (make-string-output-stream)))

(defmethod stream-finish-output ((stream log-stream))
  "Applies fontification to contents of stream and outputs it to log widget."
  (let ((string (get-output-stream-string (buffer stream))))
    (output (log-stream-log stream)
            (fontify :orange
                     (regex-replace-all "\\n" string "<br />"))))
  (clear-output stream))

(defmethod stream-force-output ((stream log-stream))
  (stream-finish-output stream))

(defmethod stream-write-string ((stream log-stream) string
                                &optional (start 0) end)
  (write-string string (buffer stream) :start start :end end)
  (stream-finish-output stream))

(defmethod stream-write-char ((stream log-stream) char)
  (write-string (string char) stream))

(defmethod stream-terpri ((stream log-stream))
  (write-char #\Newline stream))

;;
;;;; Widget
;;
;;

(define-widget main (qwidget)
  ((log-stream
     :accessor log-stream
     :documentation "The stream that holds the log output."))
  (:documentation "The main widget of the Fossicker QT GUI."))

(defun call-with-gui-stream (widget function)
  "Calls  FUNCTION  with  output  stream   variables  bound  to  LOG-STREAM  of
WIDGET.  Also ignores  errors  and outputs  error message  to  LOG-STREAM in  a
non-interactive manner."
  (let* ((*standard-output* (log-stream widget))
         (*error-output* *standard-output*)
         (*trace-output* *standard-output*))
    (handler-case (funcall function)
      (error (err) (format t (fontify-error err))))))

(defmacro with-gui-stream ((widget) &body body)
  "Wrapper macro for forwarding output to LOG-STREAM of WIDGET."
  `(call-with-gui-stream ,widget (lambda () ,@body)))

;;
;;;; Log
;;
;;

(define-subwidget (main log) (q+:make-qtextedit)
  (let ((font (q+:make-qfont "Monospace" 7)))
    ;; (setf (q+:word-wrap log) t)
    ;; (setf (q+:frame-style log)
    ;;       (logior (q+:qframe.styled-panel) (q+:qframe.sunken)))
    (setf (q+:style-hint font) (q+:qfont.type-writer))
    (setf (q+:font log) font)))

;;
;;;; Setup
;;
;;

(define-initializer (main main-setup)
  ;; (setf (q+:window-title main) "Fossicker: Open Source Asset Prospector")
  (setf (q+:fixed-size main) (values 480 520))
  (setf (log-stream main)
        (make-instance 'log-stream :log log))
  (with-gui-stream (main)
    (format t "Welcome to Fossicker!")))

;;
;;;; Header
;;
;;

(define-subwidget (main logo) (q+:make-qlabel)
  (setf (q+:pixmap logo) (q+:make-qpixmap (namestring
                                           (subpathname*
                                            *repository*
                                            "etc/fossicker-logo.png")))))

(define-subwidget (main info) (q+:make-qlabel "Select project, type file name,
press GENERATE. That's it! Check out our website and
follow us on Twitter for more libraries and games.")
  (setf (q+:word-wrap info) t))

(define-subwidget (main header) (q+:make-qhboxlayout)
  (q+:add-widget header logo)
  (q+:add-widget header info))

;;
;;;; Navbar
;;
;;

(define-subwidget (main website) (q+:make-qpushbutton "6x13 Website"))
(define-subwidget (main twitter) (q+:make-qpushbutton "Twitter"))
(define-subwidget (main documentation) (q+:make-qpushbutton "Documentation"))
(define-subwidget (main bug-report) (q+:make-qpushbutton "Report Bugs"))
(define-subwidget (main navbar) (q+:make-qhboxlayout)
  (q+:add-widget navbar website)
  (q+:add-widget navbar twitter)
  (q+:add-widget navbar documentation)
  (q+:add-widget navbar bug-report))

;;
;;;; Navbar
;;
;;

(define-subwidget (main rqststring) (q+:make-qlineedit "asset-name.png")
  (setf (q+:size-policy rqststring)
        (q+::make-qsizepolicy (q+::qsizepolicy.minimum-expanding)
                              (q+::qsizepolicy.fixed))))

(define-subwidget (main project) (q+:make-qcombobox)
  (setf (q+:size-policy project)
        (q+::make-qsizepolicy (q+::qsizepolicy.maximum)
                              (q+::qsizepolicy.fixed)))
  ;; Add items representing projects.
  (mapc (lambda (proj &aux (name (project-name proj)))
          (q+:add-item project name name))
        *project-registry*)
  ;; Set  current project  as  selected  in widget.  Disable  rqststring if  no
  ;; project selected.
  (cond (*project*
         (setf (q+:enabled rqststring) t)
         (setf (q+:current-index project)
               (q+:find-data project
                             (project-name
                              *project*))))
        (t
         (setf (q+:enabled rqststring) nil)
         (setf (q+:current-index project) -1))))

(define-slot (main project-selected) ((new-project string))
  (declare (connected project (activated string)))
  ;; Set project. Ensure rqststring enabled.
  (with-gui-stream (main)
    (set-project new-project)
    (setf (q+:enabled rqststring) t)))

(define-subwidget (main type) (q+:make-qlabel "")
  (setf (q+:size-policy type)
        (q+::make-qsizepolicy (q+::qsizepolicy.ignored)
                              (q+::qsizepolicy.fixed))))

(define-subwidget (main grid) (q+:make-qgridlayout)
  (setf (q+:size-constraint grid) (q+:qlayout.set-fixed-size))
  (setf (q+:alignment grid) (q+:qt.align-top))
  (q+:add-widget grid (q+:make-qlabel "Project") 0 0)
  (q+:add-widget grid project 0 1)  
  (q+:add-widget grid (q+:make-qlabel "Request") 0 2)
  (q+:add-widget grid rqststring 0 3)
  (q+:add-widget grid (q+:make-qlabel "Type") 1 0)
  (q+:add-widget grid type 1 1 1 3))

(define-subwidget (main initargs-scroller) (q+:make-qscrollarea)
  (setf (q+:widget-resizable initargs-scroller) t)
  (setf (q+:frame-shape initargs-scroller) (q+:qframe.no-frame)))

(define-subwidget (main initargs-layout) (q+:make-qvboxlayout)
  (q+:add-widget initargs-layout initargs-scroller))

(define-subwidget (main initargs-box) (q+:make-qgroupbox)
  (setf (q+:title initargs-box) "Initialization Arguments")
                                        ;(setf (q+:flat initargs-box) t)
  (setf (q+:layout initargs-box) initargs-layout))

(define-subwidget (main initargs-widget) (q+:make-qwidget initargs-scroller)
  (setf (q+:widget initargs-scroller) initargs-widget))

(define-subwidget (main initargs) (q+:make-qvboxlayout initargs-widget)
  (setf (q+:layout initargs-widget) initargs)
  (setf (q+:alignment initargs) (q+:qt.align-top)))

(define-slot (main name-set) ((new-name string))
  (declare (connected rqststring (text-edited string)))
  (sweep-layout initargs)
  (with-gui-stream (main)
    (let ((class (draft *project*
                        (q+:text rqststring))))
      (cond (class
             (dolist (arg (compute-initarg-properties class))
               (q+:add-widget initargs
                              (q+:make-qlineedit
                               (or (symbol-name
                                    (initarg-keyword arg)) ""))))
             (setf (q+:text type)
                   (format nil "~:(~a~)"
                           (regex-replace-all "-"
                                              (symbol-name class)
                                              " "))))
            (t
             (setf (q+:text type) "No type selected."))))))

;;
;;;; Source
;;
;;

(define-subwidget (main source) (q+:make-qlineedit "source"))
(define-subwidget (main browse) (q+:make-qpushbutton "Browse"))
(define-subwidget (main srcbar) (q+:make-qhboxlayout)
  (q+:add-widget srcbar (q+:make-qlabel "Source"))
  (q+:add-widget srcbar source)
  (q+:add-widget srcbar browse))

;;
;;;; Buttons
;;
;;

(define-subwidget (main reset) (q+:make-qpushbutton "RESET"))

(define-slot (main hit-reset) ()
  (declare (connected reset (pressed)))
  (with-gui-stream (main)
    ;; (draft *project* "bla_b_n_p_e_.png")
    (error "TEST ERROR!!!")))

(define-subwidget (main generate) (q+:make-qpushbutton "GENERATE"))
(define-subwidget (main buttons) (q+:make-qhboxlayout)
  (q+:add-widget buttons reset)
  (q+:add-widget buttons generate))

;;
;;;; Tabs
;;
;;

(define-subwidget (main general) (q+:make-qwidget))

(define-subwidget (main asset) (q+:make-qvboxlayout general)
  (q+:add-layout asset grid)
  (q+:add-widget asset initargs-box)
  (q+:add-layout asset srcbar)
  (q+:add-layout asset buttons))

(define-subwidget (main settings) (q+:make-qwidget))

(define-subwidget (main tabs) (q+:make-qtabwidget)
  (q+:add-tab tabs general "General")
  (q+:add-tab tabs settings "Settings"))

;;
;;;; Layout
;;
;;

(define-subwidget (main layout) (q+:make-qvboxlayout main)
  (q+:add-layout layout header)
  (q+:add-layout layout navbar)
  (q+:add-widget layout tabs)
  (q+:add-widget layout log))

;;
;;;; Main
;;
;;

(defun main ()
  (with-main-window (window (make-instance 'main)
                     :name "Fossicker: Open Source Asset Prospector")))
