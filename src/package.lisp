;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; package.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Buclet root directory for more info.

(in-package :cl-user)

(defpackage :pl
  (:use #:cl #:cffi #:sb-cga)
  (:export :add-child-shape :add-rigid-body :add-vertex :create-dynamics-world
           :delete-dynamics-world :delete-physics-sdk :delete-rigid-body
           :delete-shape :get-gl-matrix :nearest-points :new-bullet-sdk
           :remove-rigid-body :new-box-shape :new-capsule-shape
           :new-compound-shape :new-cone-shape :new-convex-hull-shape
           :new-cylinder-shape :new-mesh-interface :new-sphere-shape
           :new-static-plane-shape :set-orientation :set-position :set-euler
           :set-gl-matrix :set-scaling :set-linear-velocity :set-scaling
           :step-simulation :create-rigid-body :get-orientation :get-position
           :raycast))
