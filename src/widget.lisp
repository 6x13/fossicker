;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-

(in-package #:fossicker)
(in-readtable :qtools)

(define-widget main (qwidget)
  ((angle :initform 0)
   (angle-delta :initform 1)))
(define-initializer (main main-setup)
  (setf (q+:window-title main) "Fossicker: Open Source Asset Prospector")
  (setf (q+:fixed-size main) (values 480 360)))

(define-subwidget (main logo) (q+:make-qlabel)
  (setf (q+:pixmap logo) (q+:make-qpixmap "etc/fossicker-logo.png")))

(define-subwidget (main info) (q+:make-qlabel "Select project, type file name, press GENERATE. That's it! Check out our website and follow us on Twitter for more libraries and games.")
  (setf (q+:word-wrap info) t))

(define-subwidget (main header) (q+:make-qhboxlayout)
  (q+:add-widget header logo)
  (q+:add-widget header info))

(define-subwidget (main website) (q+:make-qpushbutton "6x13 Website"))
(define-subwidget (main twitter) (q+:make-qpushbutton "Twitter"))
(define-subwidget (main documentation) (q+:make-qpushbutton "Documentation"))
(define-subwidget (main report) (q+:make-qpushbutton "Report Bugs"))
(define-subwidget (main navbar) (q+:make-qhboxlayout)
  (q+:add-widget navbar website)
  (q+:add-widget navbar twitter)
  (q+:add-widget navbar documentation)
  (q+:add-widget navbar report))

(define-subwidget (main project) (q+:make-qcombobox)
  (q+:add-item project "6x13")
  (q+:add-item project "Twiniwt")
  (q+:add-item project "Test"))
(define-subwidget (main filename) (q+:make-qlineedit "asset-name.png"))
(define-subwidget (main type) (q+:make-qlabel "Type"))
(define-subwidget (main context) (q+:make-qlineedit "game/ui/"))
(define-subwidget (main grid) (q+:make-qgridlayout)
  (q+:add-widget grid (q+:make-qlabel "Project") 0 0)
  (q+:add-widget grid project 0 1)
  (q+:add-widget grid (q+:make-qlabel "Filename") 0 2)
  (q+:add-widget grid filename 0 3)
  (q+:add-widget grid (q+:make-qlabel "Type") 1 0)
  (q+:add-widget grid type 1 1)
  (q+:add-widget grid (q+:make-qlabel "Context") 1 2)
  (q+:add-widget grid context) 1 3)

(define-subwidget (main source) (q+:make-qlineedit "source"))
(define-subwidget (main browse) (q+:make-qpushbutton "Browse"))
(define-subwidget (main srcbar) (q+:make-qhboxlayout)
  (q+:add-widget srcbar (q+:make-qlabel "Source"))
  (q+:add-widget srcbar source)
  (q+:add-widget srcbar browse))

(define-subwidget (main reset) (q+:make-qpushbutton "RESET"))
(define-subwidget (main generate) (q+:make-qpushbutton "GENERATE"))
(define-subwidget (main buttons) (q+:make-qhboxlayout)
  (q+:add-widget buttons reset)
  (q+:add-widget buttons generate))

(define-subwidget (main general) (q+:make-qwidget))

(define-subwidget (main asset) (q+:make-qvboxlayout general)
  (q+:add-layout asset grid)
  (q+:add-layout asset srcbar)
  (q+:add-layout asset buttons))

(define-subwidget (main settings) (q+:make-qwidget))

(define-subwidget (main tabs) (q+:make-qtabwidget)
  (q+:add-tab tabs general "General")
  (q+:add-tab tabs settings "Settings"))

;; (define-subwidget (main group) (q+:make-qgroupbox "Asset Generation")
;;   (setf (q+:layout group) asset))

(define-subwidget (main log) (q+:make-qlabel "Report: SuccessReport: SuccessReport: SuccessReport: SuccessReport: SuccessReport: SuccessReport: SuccessReport: SuccessReport: SuccessReport: SuccessReport: SuccessReport: Success")
  (setf (q+:word-wrap log) t)
  (setf (q+:frame-style log)
        (logior (q+:qframe.styled-panel) (q+:qframe.sunken))))

(define-subwidget (main panel) (q+:make-qvboxlayout main)
  (q+:add-layout panel header)
  (q+:add-layout panel navbar)
  (q+:add-widget panel tabs)
  (q+:add-widget panel log))

;; (define-slot (main inc) ()
;;   (declare (connected increase (pressed))))
;; (define-slot (main dec) ()
;;   (declare (connected decrease (pressed))))

(defun main ()
  (with-main-window (window (make-instance 'main))))

(main)
