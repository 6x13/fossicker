;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-

(in-package :fossicker)

(defun message (&rest args)
  (apply #'format (cons t args)))

(defun prompt (&rest args)
  (apply #'y-or-n-p args))
