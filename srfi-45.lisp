;;;; srfi-45.lisp

(cl:in-package :srfi-45.internal)

;=========================================================================
; Boxes

(declaim (inline box unbox set-box!))

#-sbcl
(progn
  (defun box (x) (list x))
  (defun unbox (list) (car list))
  (defun set-box! (list item) (rplaca list item)) )

#+sbcl
(progn
  (defun box (x) (sb-ext:make-weak-pointer x))
  (defun unbox (box) (sb-ext:weak-pointer-value box))
  (defun set-box! (box item)
    (setq box (sb-ext:make-weak-pointer item))))

;=========================================================================
; Primitives for lazy evaluation:
(defmacro lazy (exp)
  `(box (cons 'lazy (lambda () ,exp))))

(defun eager (x)
  (box (cons 'eager x)))

(defmacro delay (exp)
  `(lazy (eager ,exp)))

(defun force (promise)
  (declare (optimize (debug 0) (space 3))) ;TCO
  (let ((content (unbox promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* (funcall (cdr content)))
                      (content (unbox promise)))                      ; *
                 (if (not (eql (car content) 'eager))                 ; *
                     (progn
                       (setf (car content) (car (unbox promise*)))
                       (setf (cdr content) (cdr (unbox promise*)))
                       (set-box! promise* content)))
                 (force promise))))))

; (*) These two lines re-fetch and check the original promise in case
;     the first line of the let* caused it to be forced.  For an example
;     where this happens, see reentrancy test 3 below.

;;; eof
