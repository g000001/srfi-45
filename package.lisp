;;;; package.lisp

(cl:in-package common-lisp-user)


(defpackage "https://github.com/g000001/srfi-45"
  (:use)
  (:export delay
           lazy?
           lazy
           eager
           force
           eager))


(defpackage "https://github.com/g000001/srfi-45#internals"
  (:use 
   "https://github.com/g000001/srfi-45"
   cl 
   trivial-garbage 
   fiveam 
   mbe)
  (:shadow loop))


;;; *EOF*
