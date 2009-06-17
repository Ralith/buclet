;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cffi.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Buclet root directory for more info.

(in-package :buclet)


;;; Foreign Types

(defctype pl-real :float)

(define-foreign-type pl-quaternion ()
  ()
  (:actual-type :pointer)
  (:simple-parser pl-quaternion))

(define-foreign-type pl-vector3 ()
  ()
  (:actual-type :pointer)
  (:simple-parser pl-vector3))


;;; Foreign Type Translators

(defmethod free-translated-object (pointer (value pl-quaternion) param)
  (declare (ignore param))
  (foreign-free pointer))


(defmethod free-translated-object (pointer (value pl-vector3) param)
  (declare (ignore param))
  (foreign-free pointer))


;(defmethod translate-from-foreign (pointer (type pl-vector3))
;  (list (mem-aref pointer 'pl-real 0)
;        (mem-aref pointer 'pl-real 1)
;        (mem-aref pointer 'pl-real 2)))


;(defmethod translate-from-foreign (pointer (type pl-quaternion))
;  (list (mem-aref pointer 'pl-real 0)
;        (mem-aref pointer 'pl-real 1)
;        (mem-aref pointer 'pl-real 2)
;        (mem-aref pointer 'pl-real 3)))


(defmethod translate-to-foreign (q (type pl-quaternion))
  (foreign-alloc 'pl-real :initial-contents (list (first q) (second q)
                                                  (third q) (fourth q))))


;; must be (q vector) for CLISP
(defmethod translate-to-foreign ((q simple-vector) (type pl-quaternion))
  (foreign-alloc 'pl-real :initial-contents (list (svref q 0) (svref q 1)
                                                  (svref q 2) (svref q 3))))


(defmethod translate-to-foreign (v (type pl-vector3))
  (foreign-alloc 'pl-real :initial-contents (list (first v) (second v)
                                                  (third v))))


;; must be (q vector) for CLISP
(defmethod translate-to-foreign ((v simple-vector) (type pl-vector3))
  (foreign-alloc 'pl-real :initial-contents (list (svref v 0) (svref v 1)
                                                  (svref v 2))))


;;; Foreign Functions

(defcfun ("plAddChildShape" add-child-shape)
    :void
  (compoundShapeHandle :pointer)
  (childShapeHandle :pointer)
  (childPos pl-vector3)
  (childOrn pl-quaternion))


(defcfun ("plAddRigidBody" add-rigid-body)
    :void
  (world :pointer)
  (object :pointer))


(defcfun ("plAddVertex" add-vertex)
    :void
  (cshape :pointer)
  (x pl-real)
  (y pl-real)
  (z pl-real))


(defcfun ("plCreateDynamicsWorld" create-dynamics-world)
    :pointer
  (physics-sdk-handle :pointer))


(defcfun ("plCreateRigidBody" plcrb)
    :pointer
  (user-data :pointer)
  (mass :float)
  (cshape :pointer))


(defcfun ("plDeleteDynamicsWorld" delete-dynamics-world)
    :void
  (world :pointer))


(defcfun ("plDeletePhysicsSdk" delete-physics-sdk)
    :void
  (physics-sdk :pointer))


(defcfun ("plDeleteRigidBody" delete-rigid-body)
    :void
  (cbody :pointer))


(defcfun ("plDeleteShape" delete-shape)
    :void
  (cshape :pointer))


(defcfun ("plGetOpenGLMatrix" get-opengl-matrix)
    :void
  (object :pointer)
  (matrix :pointer))  ; plReal* matrix


(defcfun ("plGetOrientation" plgo)
    :void
  (object :pointer)
  (orientation :pointer))  ; typedef plReal plQuaternion[4]


(defcfun ("plGetPosition" plgp)
    :void
  (object :pointer)
  (position :pointer))  ; typedef plReal plVector3[3]


;; XXX: I've never used this so it probably won't work.
;; double plNearestPoints (float p1[3], float p2[3], float p3[3], float q1[3],
;;                         float q2[3], float q3[3], float *pa, float *pb,
;;                         float normal[3])
(defcfun ("plNearestPoints" nearest-points)
    :double
  (p1 :pointer) (p2 :pointer) (p3 :pointer)
  (q1 :pointer) (q2 :pointer) (q3 :pointer)
  (pa :pointer) (pb :pointer)
  (normal :pointer))


(defcfun ("plNewBulletSdk" new-bullet-sdk)
    :pointer)


(defcfun ("plRemoveRigidBody" remove-rigid-body)
    :void
  (world :pointer)
  (object :pointer))


(defcfun ("plNewBoxShape" new-box-shape)
    :pointer
  (x pl-real)
  (y pl-real)
  (z pl-real))


(defcfun ("plNewCapsuleShape" new-capsule-shape)
    :pointer
  (radius pl-real)
  (height pl-real))


(defcfun ("plNewCompoundShape" new-compound-shape)
    :pointer)


(defcfun ("plNewConeShape" new-cone-shape)
    :pointer
  (radius pl-real)
  (height pl-real))


(defcfun ("plNewConvexHullShape" new-convex-hull-shape)
    :pointer)


(defcfun ("plNewCylinderShape" new-cylinder-shape)
    :pointer
  (radius pl-real)
  (height pl-real))


(defcfun ("plNewMeshInterface" new-mesh-interface)
    :pointer)


(defcfun ("plNewSphereShape" new-sphere-shape)
    :pointer
  (radius pl-real))


(defcfun ("plNewStaticPlaneShape" new-static-plane-shape)
    :pointer
  (plane-normal pl-vector3)
  (plane-constant :float))


(defcfun ("plSetLinearVelocity" set-linear-velocity)
    :void
  (object :pointer)
  (velocity pl-vector3))


(defcfun ("plSetOrientation" set-orientation)
    :void
  (object :pointer)
  (orientation pl-quaternion))


(defcfun ("plSetPosition" set-position)
    :void
  (object :pointer)
  (position pl-vector3))


(defcfun ("plSetEuler" set-euler)
    :void
  (yaw pl-real)
  (pitch pl-real)
  (roll pl-real)
  (orient pl-quaternion))


(defcfun ("plSetScaling" set-scaling)
    :void
  (cshape :pointer)
  (cscaling pl-vector3))


(defcfun ("plStepSimulation" step-simulation)
    :void
  (world :pointer)
  (time-step pl-real))


;;; Interfaces

(defun create-rigid-body (mass cshape)
  ;; "user-data" (the first argument below) is not used by Bullet according
  ;; to its source but if I pass a null pointer it crashes so I just let it
  ;; point to the cshape
  (plcrb cshape mass cshape))


(defun get-orientation (object)
  (with-foreign-object (array 'pl-real 4)
    (plgo object array)
    (list (mem-aref array 'pl-real 0)
          (mem-aref array 'pl-real 1)
          (mem-aref array 'pl-real 2)
          (mem-aref array 'pl-real 3))))


(defun get-position (object)
  (with-foreign-object (array 'pl-real 3)
    (plgp object array)
    (list (mem-aref array 'pl-real 0)
          (mem-aref array 'pl-real 1)
          (mem-aref array 'pl-real 2))))
