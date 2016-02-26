;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FOSSICKER -*-
;; Copyright (C) 2015 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 23 October 2015

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

(in-package :fossicker)

(declaim (optimize (speed 0)
                   (compilation-speed 0)
                   (safety 3)
                   (debug 3)))

;;;;;;;;;;;;;;;
;;; Interactive
;;
;;
;;;; Interactive Object & Class
;;
;;

(defclass interactive-object (cl:standard-object)
  ())

(defclass interactive-class (cl:standard-class)
  ((interactive
    :type list
    :initarg :interactive
    :reader class-interactive
    :initform nil))
  (:default-initargs
   :direct-superclasses (list (find-class 'interactive-object))))

(defmethod validate-superclass ((class interactive-class)
                                (superclass cl:standard-class))
  t)

(defmethod partial-class-base-initargs append ((class interactive-class))
  '(:interactive))

(defmethod initialize-instance :around
    ((class interactive-class) &rest initargs
     &key direct-superclasses)
  (if (loop for superclass in direct-superclasses
              thereis (subclassp superclass 'interactive-object))
      (call-next-method)
      (apply #'call-next-method class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'interactive-object)))
             initargs)))

(defmethod reinitialize-instance :around
    ((class interactive-class) &rest initargs
     &key (direct-superclasses () direct-superclasses-p))
  (if direct-superclasses-p
      (if (loop for superclass in direct-superclasses
                  thereis (subclassp superclass 'interactive-object))
          (call-next-method)
          (apply #'call-next-method class
                 :direct-superclasses
                 (append direct-superclasses
                         (list (find-class 'interactive-object)))
                 initargs))
      (call-next-method)))

;;
;;;; Interactive Slot Definitions
;;
;;

(defgeneric slot-definition-interactivep (slot)
  (:documentation "Predicate to check is slot is defined as interactive."))

(defmethod slot-definition-interactivep ((slot slot-definition))
  nil)

(defclass interactive-direct-slot-definition
    (standard-direct-slot-definition)
  ((interactivep :initarg :interactive
                 :initform nil
                 :reader slot-definition-interactivep)))

(defclass interactive-effective-slot-definition
    (standard-effective-slot-definition)
  ())

(defmethod slot-definition-interactivep
    ((slot interactive-effective-slot-definition))
  t)

(defmethod direct-slot-definition-class
    ((class interactive-class) &key &allow-other-keys)
  (find-class 'interactive-direct-slot-definition))

(defvar *interactive-effective-slot-definition-class*)

(defmethod effective-slot-definition-class
    ((class interactive-class) &key &allow-other-keys)
  (if *interactive-effective-slot-definition-class*
      *interactive-effective-slot-definition-class*
      (call-next-method)))

(defmethod compute-effective-slot-definition
    ((class interactive-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((*interactive-effective-slot-definition-class*
          (when (some #'slot-definition-interactivep direct-slot-definitions)
            (find-class 'interactive-effective-slot-definition))))
    (call-next-method)))

;;
;;;; Interactive Special Layered Access Class
;;
;;

(defclass interactive-special-layered-access-class
    (special-layered-access-class
     interactive-class)
  ())

;;
;;;; Interactive Special Layered Slot Definitions
;;
;;

(defclass interactive-special-layered-direct-slot-definition
    (special-layered-direct-slot-definition
     interactive-direct-slot-definition
     standard-direct-slot-definition-in-layer)
  ())

(defclass interactive-effective-slot-definition-in-layers
    (interactive-effective-slot-definition
     standard-effective-slot-definition-in-layers)
  ())

(defclass interactive-layered-effective-slot-definition
    (layered-effective-slot-definition-in-layers
     interactive-effective-slot-definition-in-layers)
  ())

(defmethod direct-slot-definition-class
    ((class interactive-special-layered-access-class) &key &allow-other-keys)
  (find-class 'interactive-special-layered-direct-slot-definition))

(defvar *interactive-layered-effective-slot-definition-class*)

(defmethod effective-slot-definition-class
    ((class interactive-special-layered-access-class) &key &allow-other-keys)
  (if *interactive-layered-effective-slot-definition-class*
      *interactive-layered-effective-slot-definition-class*
      (call-next-method)))

(defmethod compute-effective-slot-definition
    ((class interactive-special-layered-access-class)
     name direct-slot-definitions)
  (declare (ignore name))
  (let ((*interactive-layered-effective-slot-definition-class*
          (if (some #'slot-definition-interactivep direct-slot-definitions)
              (if (some #'slot-definition-layeredp direct-slot-definitions)
                  (find-class
                   'interactive-layered-effective-slot-definition)
                  (find-class
                   'interactive-effective-slot-definition-in-layers)))))
    (call-next-method)))

;;
;;;; Exposure
;;
;;

(defclass exposure (layered-class
                    interactive-special-layered-access-class)
  ()
  (:default-initargs
   :defining-metaclass 'interactive-special-layered-access-class))

(defmethod validate-superclass ((class exposure)
                                (superclass cl:standard-class)) 
  t)

;;
;;;; Exposing Functions
;;
;;

(defun document-slot (slot)
  (documentation slot 't))

(deftype slot-or-null ()
  "Type definition for slot definition query."
  '(or null standard-effective-slot-definition))

(defun infer-presentation (type)
  "Infers how to present the INTERACTION from type."
  nil)

(defgeneric infer-initarg-properties (class &rest initargs)
  (:documentation "Loops through the provided  INITARGS, returning a plist with
values  consisting  of  :TYPE,   :INITFORM,  :DOCUMENTATION  and  :PRESENTATION
information for each INITARG as key.")
  (:method ((class symbol) &rest initargs)
    (apply #'infer-initarg-properties (find-class class) initargs))
  (:method ((class exposure) &rest initargs)
    (ensure-finalized class)
    (loop
      ;; Get DEFAULT-INITARGS and SLOTS of CLASS.
      with default-initargs of-type list = (class-default-initargs class)
      and slots of-type list = (class-slots class)
      ;; Loop in INITARGS.
      for initarg of-type keyword in initargs
      ;; Find DEFAULT-INITARG and/or SLOT the INITARG belongs to, if any.
      as default-initarg of-type list = (find initarg default-initargs :key #'car)
      and slot of-type slot-or-null = (find initarg slots
                                            :key #'slot-definition-initargs
                                            :test #'member)
      ;; Find relevant values present in above structures.
      as default-initform = (cadr default-initarg)
      as slot-initform = (if slot (slot-definition-initform slot))
      and slot-type = (if slot (slot-definition-type slot))
      and slot-documentation = (if slot (document-slot slot))
      ;; Append all into a PLIST.
      appending
      (list initarg (list :type slot-type
                          :presentation (infer-presentation slot-type)
                          :initform (or default-initform slot-initform)
                          :documentation slot-documentation)))))

(defgeneric compute-initarg-properties (class)
  (:documentation  "Loops  through the  provided  INTERACTIVE  slots of  CLASS,
returning a PLIST with values consisting of :INITFORM and :TYPE information for
each INITARG as key.")
  (:method ((class symbol))
    (compute-initarg-properties (find-class class)))
  (:method ((class exposure)
            &aux (keys (class-interactive class))
              (properties (apply #'infer-initarg-properties class keys)))
    properties
    ;; (loop for key of-type keyword in keys
    ;;       collecting (getf properties key)
    ))
