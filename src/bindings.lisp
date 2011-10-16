;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cffi.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Buclet root directory for more info.

(in-package :pl)


;;; Foreign Types

(defctype %real :float)
(defctype quaternion (:pointer %real))
(defctype vector3 (:pointer %real))
(defctype matrix (:pointer %real))

(defcenum broadphase
  :sap
  :dbvt)

;;; Foreign Functions
(defmacro with-pointers-to-vector-data ((&rest clauses) &body body)
  (labels ((build-result (c b)
             (if c
                 `((with-pointer-to-vector-data ,(first c)
                     ,@(build-result (rest c) b)))
                 b)))
    (first (build-result clauses body))))

(eval-when (:compile-toplevel)
  (defun shareable? (ctype)
    (or (eq ctype 'vector3)
        (eq ctype 'quaternion))))

(defmacro defcfun* ((cname lname) return &body args)
  (let ((binding-name (copy-symbol lname)))
   `(progn (defcfun (,cname ,binding-name) ,return
             ,@args)
           (defun ,lname ,(mapcar #'first args)
             (with-pointers-to-vector-data ,(mapcar
                                             (lambda (x)
                                               (list (first x) (first x)))
                                             (remove-if-not #'shareable?
                                                            args :key #'second))
               (,binding-name ,@(mapcar #'first args)))))))

(defcfun* ("plAddChildShape" add-child-shape)
    :void
  (compoundShapeHandle :pointer)
  (childShapeHandle :pointer)
  (childPos vector3)
  (childOrn quaternion))


(defcfun* ("plAddRigidBody" add-rigid-body)
    :void
  (world :pointer)
  (object :pointer))


(defcfun* ("plAddVertex" add-vertex)
    :void
  (cshape :pointer)
  (x %real)
  (y %real)
  (z %real))


(defcfun* ("plCreateDynamicsWorld" create-dynamics-world) :pointer
  (physics-sdk-handle :pointer)
  (broadphase broadphase))


(defcfun* ("plCreateRigidBody" plcrb)
    :pointer
  (user-data :pointer)
  (mass :float)
  (cshape :pointer))


(defcfun* ("plDeleteDynamicsWorld" delete-dynamics-world)
    :void
  (world :pointer))


(defcfun* ("plDeletePhysicsSdk" delete-physics-sdk)
    :void
  (physics-sdk :pointer))


(defcfun* ("plDeleteRigidBody" delete-rigid-body)
    :void
  (cbody :pointer))


(defcfun* ("plDeleteShape" delete-shape)
    :void
  (cshape :pointer))


(defcfun* ("plGetOpenGLMatrix" get-gl-matrix)
    :void
  (object :pointer)
  (matrix matrix))  ; pl%Real* matrix


(defcfun* ("plGetOrientation" %get-orientation)
    :void
  (object :pointer)
  (orientation quaternion))


(defcfun* ("plGetPosition" %get-position)
    :void
  (object :pointer)
  (position vector3))  ; typedef pl%Real plVector3[3]


;; XXX: I've never used this so it probably won't work.
;; double plNearestPoints (float p1[3], float p2[3], float p3[3], float q1[3],
;;                         float q2[3], float q3[3], float *pa, float *pb,
;;                         float normal[3])
(defcfun* ("plNearestPoints" nearest-points)
    :double
  (p1 :pointer) (p2 :pointer) (p3 :pointer)
  (q1 :pointer) (q2 :pointer) (q3 :pointer)
  (pa :pointer) (pb :pointer)
  (normal vector3))


(defcfun* ("plNewBulletSdk" new-bullet-sdk)
    :pointer)


(defcfun* ("plRemoveRigidBody" remove-rigid-body)
    :void
  (world :pointer)
  (object :pointer))


(defcfun* ("plNewBoxShape" new-box-shape)
    :pointer
  (x %real)
  (y %real)
  (z %real))


(defcfun* ("plNewCapsuleShape" new-capsule-shape)
    :pointer
  (radius %real)
  (height %real))


(defcfun* ("plNewCompoundShape" new-compound-shape)
    :pointer)


(defcfun* ("plNewConeShape" new-cone-shape)
    :pointer
  (radius %real)
  (height %real))


(defcfun* ("plNewConvexHullShape" new-convex-hull-shape)
    :pointer)


(defcfun* ("plNewCylinderShape" new-cylinder-shape)
    :pointer
  (radius %real)
  (height %real))


(defcfun* ("plNewMeshInterface" new-mesh-interface)
    :pointer)


(defcfun* ("plNewSphereShape" new-sphere-shape)
    :pointer
  (radius %real))


(defcfun* ("plNewStaticPlaneShape" new-static-plane-shape)
    :pointer
  (plane-normal vector3)
  (plane-constant :float))


(defcfun* ("plSetLinearVelocity" set-linear-velocity)
    :void
  (object :pointer)
  (velocity vector3))


(defcfun* ("plSetOrientation" set-orientation)
    :void
  (object :pointer)
  (orientation quaternion))


(defcfun* ("plSetPosition" set-position)
    :void
  (object :pointer)
  (position vector3))


(defcfun* ("plSetEuler" set-euler)
    :void
  (yaw %real)
  (pitch %real)
  (roll %real)
  (orient quaternion))

(defcfun* ("plSetOpenGLMatrix" set-gl-matrix) :void
  (object :pointer)
  (matrix matrix))

(defcfun* ("plSetScaling" set-scaling)
    :void
  (cshape :pointer)
  (cscaling vector3))

(defcfun* ("plRayCast" %raycast) :pointer
  (world :pointer)
  (ray-start vector3)
  (ray-end vector3)
  (pos vector3)
  (normal vector3))

(defcfun* ("plStepSimulation" step-simulation)
    :void
  (world :pointer)
  (dt %real)
  (max-substeps :int)
  (time-step %real))


;;; Interfaces

(defun create-rigid-body (mass cshape)
  ;; "user-data" (the first argument below) is not used by Bullet according
  ;; to its source but if I pass a null pointer it crashes so I just let it
  ;; point to the cshape
  (plcrb cshape mass cshape))


(defun get-orientation (object &optional (result (make-array 4 :element-type 'single-float)))
  (%get-orientation object result)
  result)

(defun get-position (object &optional (result (vec 0.0 0.0 0.0)))
  (%get-position object result)
  result)

(defun raycast (world start end &optional (pos (vec 0.0 0.0 0.0))
                                  (normal (vec 0.0 0.0 0.0)))
  (let ((object (%raycast world start end pos normal)))
    (unless (null-pointer-p object)
      (values object pos normal))))
