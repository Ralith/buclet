;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; hello-world.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.
;;;;
;;;; See: http://www.continuousphysics.com/mediawiki-1.5.8/index.php?title=Hello_World

;;; Packages

;(asdf :buclet)
(defpackage :pl-hello-world
  (:use :cl :pl :sb-cga))
(in-package :pl-hello-world)


;;; Variables

(defparameter *dynamics-world* nil)
(defparameter *fall-rigid-body* nil)
(defparameter *fall-shape* nil)
(defparameter *ground-rigid-body* nil)
(defparameter *ground-shape* nil)
(defparameter *physics-sdk* nil)


;;; Functions

(defun quat (a b c d)
  (make-array 4 :element-type 'single-float :initial-contents (list a b c d)))

(defun start-simulation ()
  (set-position *fall-rigid-body* (vec 0.0 50.0 0.0))
  (set-orientation *fall-rigid-body* (quat 0.0 0.0 0.0 1.0))
  (loop for i from 0 to 300
        with sim-step = (/ 1.0 60.0)
        do (step-simulation *dynamics-world* sim-step 10 sim-step)
           (format t "~A: sphere Y position: ~A~%"
                   i (aref (get-position *fall-rigid-body*) 1))
           (sleep sim-step)))


;;; Initialisation

(setf *physics-sdk* (new-bullet-sdk))
(setf *dynamics-world* (create-dynamics-world *physics-sdk* :dbvt))

(setf *ground-shape* (new-static-plane-shape (vec 0.0 1.0 0.0) 1.0))
(setf *ground-rigid-body* (create-rigid-body 0.0 *ground-shape*))
(set-position *ground-rigid-body* (vec 0.0 -1.0 0.0))
(set-orientation *ground-rigid-body* (quat 0.0 0.0 0.0 1.0))
(add-rigid-body *dynamics-world* *ground-rigid-body*)

(setf *fall-shape* (new-sphere-shape 1.0))
(setf *fall-rigid-body* (create-rigid-body 1.0 *fall-shape*))
(set-position *fall-rigid-body* (vec 0.0 50.0 0.0))
(set-orientation *fall-rigid-body* (quat 0.0 0.0 0.0 1.0))
(add-rigid-body *dynamics-world* *fall-rigid-body*)


;;; Main Program

;(start-simulation)
