;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 20 October 2015

;; This file is part of Fossicker.

;; Fossicker is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Fossicker is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Fossicker.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:fossicker-widget)
(in-readtable :qtools)

;;
;;;; Output
;;
;;

(defun output (log format-string &rest args)
  (q+:move-cursor log (q+:qtextcursor.end))
  (q+:insert-html log (apply #'format NIL format-string args))
  (q+:move-cursor log (q+:qtextcursor.end)))

(defun escape (text)
  (flet ((r (text find replace)
           (cl-ppcre:regex-replace-all find text replace)))
    (r (r (r text "&" "&amp;") "<" "&lt;") ">" "&gt;")))

(defun output-colored (log color format-string &rest args)
  (output log "<span style=\"color:~a;\">~a</span>" color (apply #'format NIL format-string args)))

(defun output-comment (log format-string &rest args)
  (output-colored log "gray" "; ~a<br />" (apply #'format NIL format-string args)))

(defun output-error (log error)
  (output log "<br />")
  (output-comment log "<span style=\"color:red;\">Error:</span> ~a" (escape (princ-to-string error)))
  (output-comment log "[Condition of type ~a]" (escape (princ-to-string (type-of error)))))

;;
;;;; Stream
;;
;;

(defclass log-stream (fundamental-character-output-stream trivial-gray-stream-mixin)
  ((log :initarg :log :initform (error "LOG required.") :accessor log-stream-log)
   (buffer :initform (make-string-output-stream) :accessor buffer)))

(defmethod stream-clear-output ((stream log-stream))
  (setf (buffer stream) (make-string-output-stream)))

(defmethod stream-finish-output ((stream log-stream))
  (let ((string (get-output-stream-string (buffer stream))))
    (output-colored (log-stream-log stream) "orange" "~a" (cl-ppcre:regex-replace-all "\\n" (escape string) "<br />")))
  (clear-output stream))

(defmethod stream-force-output ((stream log-stream))
  (stream-finish-output stream))

(defmethod stream-write-string ((stream log-stream) string &optional (start 0) end)
  (write-string string (buffer stream) :start start :end end)
  (stream-finish-output stream))

(defmethod stream-write-char ((stream log-stream) char)
  (write-string (string char) stream))

(defmethod stream-terpri ((stream log-stream))
  (write-char #\Newline stream))


;;;;;;;;;;;;;;;;;;;;
;;; Fossicker Widget
;;
;;

(define-widget main (qwidget)
  ((log-stream :accessor log-stream)))

;;
;;;; Log
;;
;;

(define-subwidget (main log) (q+:make-qtextedit)
  (let ((font (q+:make-qfont "Monospace" 8)))
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
  (setf (q+:window-title main) "Fossicker: Open Source Asset Prospector")
  (setf (q+:fixed-size main) (values 480 520))
  (setf (log-stream main)
        (make-instance 'log-stream :log log)))

;;
;;;; Header
;;
;;

(define-subwidget (main logo) (q+:make-qlabel)
  (setf (q+:pixmap logo) (q+:make-qpixmap (namestring
                                           (subpathname*
                                            *repository*
                                            "etc/fossicker-logo.png")))))

(define-subwidget (main info) (q+:make-qlabel "Select project, type file name, press GENERATE. That's it! Check out our website and follow us on Twitter for more libraries and games.")
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

(define-subwidget (main project) (q+:make-qcombobox)
  (setf (q+:size-policy project)
        (q+::make-qsizepolicy (q+::qsizepolicy.maximum)
                              (q+::qsizepolicy.fixed)))
  (q+:add-item project "6x13")
  (q+:add-item project "Twiniwt")
  (q+:add-item project "Test"))

(define-subwidget (main namestring) (q+:make-qlineedit "asset-name.png")
  (setf (q+:size-policy namestring)
        (q+::make-qsizepolicy (q+::qsizepolicy.minimum-expanding)
                              (q+::qsizepolicy.fixed))))

(define-subwidget (main type) (q+:make-qlabel "Type")
  (setf (q+:size-policy type)
        (q+::make-qsizepolicy (q+::qsizepolicy.ignored)
                              (q+::qsizepolicy.fixed))))

(define-subwidget (main grid) (q+:make-qgridlayout)
  (setf (q+:size-constraint grid) (q+:qlayout.set-fixed-size))
  (setf (q+:alignment grid) (q+:qt.align-top))
  (q+:add-widget grid (q+:make-qlabel "Project") 0 0)
  (q+:add-widget grid project 0 1)  
  (q+:add-widget grid (q+:make-qlabel "Namestring") 0 2)
  (q+:add-widget grid namestring 0 3)
  (q+:add-widget grid (q+:make-qlabel "Type") 1 0)
  (q+:add-widget grid type 1 1))

(define-subwidget (main initargs-scroller) (q+:make-qscrollarea)
  ;; (setf (q+:background-role initargs-scroller) (q+:qpalette.dark))
  (setf (q+:widget-resizable initargs-scroller) t)
  (setf (q+:frame-shape initargs-scroller) (q+:qframe.no-frame)))

(define-subwidget (main initargs-layout) (q+:make-qvboxlayout)
  (q+:add-widget initargs-layout initargs-scroller)
  ;; (q+:add-stretch initargs-layout 0)
  )

(define-subwidget (main initargs-box) (q+:make-qgroupbox)
  (setf (q+:title initargs-box) "Initialization Arguments")
  ;(setf (q+:flat initargs-box) t)
  (setf (q+:layout initargs-box) initargs-layout))

(define-subwidget (main initargs-widget) (q+:make-qwidget initargs-scroller)
  (setf (q+:widget initargs-scroller) initargs-widget))

(define-subwidget (main initargs) (q+:make-qvboxlayout initargs-widget)
  (setf (q+:layout initargs-widget) initargs)
  (setf (q+:alignment initargs) (q+:qt.align-top)))

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

;; (define-subwidget (main group) (q+:make-qgroupbox "Asset Generation")
;;   (setf (q+:layout group) asset))

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
;;;; Slots
;;
;;

;; (define-slot (main hit-reset) ()
;;   (declare (connected reset (pressed)))
;;   (loop for widget = (q+:take-at initargs 0) until (null-qobject-p widget)
;;         do (progn (format t "~A" widget) (#_delete widget))))

(define-slot (main hit-reset) ()
  (declare (connected reset (pressed)))
  (let* ((*standard-output* (log-stream main))
         (*error-output* *standard-output*)
         (*trace-output* *standard-output*))
    (handler-case
        (progn
          (fossicker:set-project "6x13")
          (fossicker::draft *project* "bla_b_n_p_e_.png")
          (error "TEST ERROR!!! TEST ERROR!!! TEST ERROR!!! TEST ERROR!!! TEST ERROR!!! TEST ERROR!!! TEST ERROR!!!"))
      (error (err) (output-error log err))))
  (sweep-layout initargs))

(define-signal (main name-set) (string))
(define-slot (main name-set) ((new-name string))
  (declare (connected main (name-set string)))
  (q+:add-widget initargs (q+:make-qlineedit "Project"))
  (setf (q+:text source) (q+:text namestring)))

(define-slot (main go) ()
  (declare (connected namestring (text-edited string)))
  (signal! main (name-set string) (q+:text namestring)))

;; (define-slot (main inc) ()
;;   (declare (connected increase (pressed))))
;; (define-slot (main dec) ()
;;   (declare (connected decrease (pressed))))

;;
;;;; Widget
;;
;;

(defun main ()
  (with-main-window (window (make-instance 'main))))
