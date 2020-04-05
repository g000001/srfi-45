;;;; srfi-45.lisp

(cl:in-package "https://github.com/g000001/srfi-45#internals")

;=========================================================================
; Boxes

(declaim (notinline box unbox set-box!))

;(defun box (x) (make-weak-pointer (list x)))
(defun box (x) (list x))

;(defun unbox (box) (car (weak-pointer-value box)))
(defun unbox (box) (car box))
#|(defun set-box! (box item)
  (rplaca (weak-pointer-value box)
          item))|#
(defun set-box! (box item)
  (rplaca box item))


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

#|(defun force (promise)
  (prog (content promise*)
      L (setq content (unbox promise))
        (return
         (case (car content)
           ((eager) (cdr content))
           ((lazy)  
            (setq promise* (funcall (cdr content)))
            (setq content (unbox promise))
            (when (not (eql (car content) 'eager))                 ; *
              (setf (car content) (car (unbox promise*)))
              (setf (cdr content) (cdr (unbox promise*)))
              (set-box! promise* content))
            (go L))))))|#

;;; eof
