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

(in-package :fossicker)

;;;;;;;;;;;;;;;;;;;;
;;; Server Interface
;;
;;

;; (defun start-simple-server (port)
;;   "Listening on a port for a message, and print the received message."
;;   (usocket:with-socket-listener (socket "127.0.0.1" port)
;;     (usocket:wait-for-input socket)
;;     (usocket:with-connected-socket (connection (usocket:socket-accept socket))
;; 	  (format t "~a~%" (read-line (usocket:socket-stream connection))))))

;; (defun start-simple-client (port)
;;   "Connect to a server and send a message."
;;   (usocket:with-client-socket (socket stream "127.0.0.1" port)
;;     (format stream "Hello world!~%")
;; (force-output stream)))
