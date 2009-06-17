;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; buclet.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Buclet root directory for more info.

(in-package :cl-user)

(defpackage :buclet-system
  (:use :cl :asdf))

(in-package :buclet-system)

(asdf:defsystem :buclet
  :version "2.74.1"
  :components
    ((:module src
      :components
        ((:file "package")
         (:file "buclet" :depends-on ("package"))
         (:file "cffi" :depends-on ("package" "buclet")))))
  :depends-on (:cffi))
