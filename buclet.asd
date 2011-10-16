;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; buclet.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Buclet root directory for more info.

(in-package :cl-user)

(asdf:defsystem :buclet
  :version "2.74.1"
  :depends-on (#:cffi #:sb-cga)
  :components
    ((:module "src"
      :components
        ((:file "package")
         (:file "libraries" :depends-on ("package"))
         (:file "bindings" :depends-on ("package" "libraries"))))))
