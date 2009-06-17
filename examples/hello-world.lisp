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

(asdf :buclet)
(in-package :buclet)


;;; Variables

(defparameter *dynamics-world* nil)
(defparameter *fall-rigid-body* nil)
(defparameter *fall-shape* nil)
(defparameter *ground-rigid-body* nil)
(defparameter *ground-shape* nil)
(defparameter *physics-sdk* nil)


;;; Functions

(defun start-simulation ()
  (set-position *fall-rigid-body* '(0.0 50.0 0.0))
  (set-orientation *fall-rigid-body* '(0.0 0.0 0.0 1.0))
  (loop for i from 0 to 300
        with sim-step = (/ 1.0 60.0)
        do (step-simulation *dynamics-world* sim-step)
           (format t "~A: sphere Y position: ~A~%"
                   i (second (get-position *fall-rigid-body*)))
           (sleep sim-step)))


;;; Initialisation

(setf *physics-sdk* (new-bullet-sdk))
(setf *dynamics-world* (create-dynamics-world *physics-sdk*))

(setf *ground-shape* (new-static-plane-shape '(0.0 1.0 0.0) 1.0))
(setf *ground-rigid-body* (create-rigid-body 0.0 *ground-shape*))
(set-position *ground-rigid-body* '(0.0 -1.0 0.0))
(set-orientation *ground-rigid-body* '(0.0 0.0 0.0 1.0))
(add-rigid-body *dynamics-world* *ground-rigid-body*)

(setf *fall-shape* (new-sphere-shape 1.0))
(setf *fall-rigid-body* (create-rigid-body 1.0 *fall-shape*))
(set-position *fall-rigid-body* '(0.0 50.0 0.0))
(set-orientation *fall-rigid-body* '(0.0 0.0 0.0 1.0))
(add-rigid-body *dynamics-world* *fall-rigid-body*)


;;; Main Program

(start-simulation)
