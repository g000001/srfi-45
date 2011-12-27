;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-45
  (:use)
  (:export :delay
           :lazy?
           :lazy
           :force
           :eager))

(defpackage :srfi-45.internal
  (:use :srfi-45 :cl :mbe :fiveam)
  (:shadow :loop))
