;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-

(in-package :fossicker)

(defun message (&rest args)
  (format t "~A~%" (apply #'format nil args)))

(defun prompt (&rest args)
  (apply #'y-or-n-p args))
