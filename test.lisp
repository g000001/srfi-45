(cl:in-package :srfi-45.internal)

(def-suite srfi-45)

(in-suite srfi-45)

(defmacro progn-with-output-to-string ((out) &body body)
  `(let ((,out (make-string-output-stream)))
     (values
      (progn ,@body)
      (get-output-stream-string ,out))))

(defmacro until-stack-exhausted-or-timeout (&body body)
  #+sbcl `(handler-case (sb-ext:with-timeout 1 ,@body)
            (sb-kernel::control-stack-exhausted ()
              'stack-exhausted)
            (sb-ext:timeout () 'timeout))
  #-sbcl t)

;=========================================================================
; TESTS AND BENCHMARKS:
;=========================================================================

;=========================================================================
(test |Memoization test 1:|
  (is (equal (let* ((out (make-string-output-stream))
                    (s (delay (progn (princ 'hello out) 1))))
               (list (force s)
                     (force s)
                     (get-output-stream-string out)))
             '(1 1 "HELLO"))))
               ;===> Should display 'hello once


;=========================================================================
(test |Memoization test 2:|
  (is (equal (let ((out (make-string-output-stream)))
               (list (let ((s (delay (progn (princ 'bonjour out) 2))))
                       (+ (force s) (force s)) )
                     (get-output-stream-string out) ))
             '(4 "BONJOUR") )))
               ;===> Should display 'bonjour once

;=========================================================================
(test |Memoization test 3: (pointed out by Alejandro Forero Cuervo)|
  (is (equal
       (let* ((out (make-string-output-stream))
              (r (delay (progn (princ 'hi out) 1)))
              (s (lazy r))
              (\t (lazy s)) )
         (list (force \t)
               (force r)
               (get-output-stream-string out) ))
       '(1 1 "HI") )))

               ;===> Should display 'hi once

;=========================================================================
(test |Memoization test 4: Stream memoization|
  (defun stream-drop (s index)
    (lazy
     (if (zerop index)
       s
       (stream-drop (cdr (force s)) (- index 1)))))

  (defun ones ()
    (delay (progn
           (princ 'ho *standard-output*)
           (cons 1 (ones)))))

  (is (equal (let ((s (ones)))
               (multiple-value-list
                (progn-with-output-to-string (*standard-output*)
                  (car (force (stream-drop s 4)))
                  (car (force (stream-drop s 4))) )))
             '(1 "HOHOHOHOHO") )))
               ;===> Should display 'ho five times

;=========================================================================
(test |Reentrancy test 1: from R5RS|
  (let* ((x 5)
         (count 0)
         p)
    (setq p (delay (progn (setq count (+ count 1))
                          (if (> count x)
                              count
                              (force p)))))
    (is (= 6 (force p)))
                     ;===>  6
    (setq x 10)
    (is (= 6 (force p)))))
                     ;===>  6

;=========================================================================
(test |Reentrancy test 2: from SRFI 40|
  (let (f)
    (setq f
          (let ((first? t))
            (delay
             (if first?
                 (progn
                   (setq first? nil)
                   (force f))
                 'second))))
    (is (eq 'second (force f)))))
                     ;===> 'second

;=========================================================================
(test |Reentrancy test 3: due to John Shutt|
  (let (p q)
    (setq q (let ((count 5))
              (defun get-count () count)
              (setq p (delay (if (<= count 0)
                                 count
                                 (progn (setq count (- count 1))
                                        (force p)
                                        (setq count (+ count 2))
                                        count))))
              (list #'get-count p)))
    (setf (symbol-function 'get-count)
          (car q))
    (setq p (cadr q))

    (is (= 5 (funcall #'get-count)))
              ; =>   5
    (is (= 0 (force p)))
              ; =>   0
    (is (= 10 (funcall #'get-count)))))
              ; =>   10

;=========================================================================
; Test leaks:  All the leak tests should run in bounded space.

;=========================================================================
(defun loop () (lazy (loop)))

(test |Leak test 1: Infinite loop in bounded space.|
  (is-false (eq 'stack-exhausted
                (until-stack-exhausted-or-timeout
                  (force (loop))))))

;=========================================================================
(test |Leak test 2: Pending memos should not accumulate
              in shared structures.|
  (is-false  (eq 'stack-exhausted
                 (until-stack-exhausted-or-timeout
                   (let (s)
                     (setq s (loop))
                     (force s) ;==> bounded space
                     )))))

;=========================================================================
(defun from (n)
  (delay (cons n (from (+ n 1)))))

(defun traverse (s)
  (lazy (traverse (cdr (force s)))))

(test |Leak test 3: Safely traversing infinite stream.|
  (is-false (eq 'stack-exhausted
                (until-stack-exhausted-or-timeout
                  (force (traverse (from 0)))))))

;=========================================================================
(test |Leak test 4: Safely traversing infinite stream
              while pointer to head of result exists.|
  (is-false (eq 'stack-exhausted
                (until-stack-exhausted-or-timeout
                  (let (s)
                    (setq s (traverse (from 0)))
                    (force s))))))

;=========================================================================
; Convenient list deconstructor used below.

(define-syntax match
  (syntax-rules ()
    ((match exp
       (()      exp1)
       ((h . \t) exp2))
     (let ((lst exp))
       (cond ((null lst) exp1)
             ((consp lst) (let ((h (car lst))
                                 (\t (cdr lst)))
                            exp2))
             (:else 'match-error))))))

;========================================================================
(defun stream-filter (p? s)
  (lazy (match (force s)
          (()      (delay '()))
          ((h . \t) (if (funcall p? h)
                       (delay (cons h (stream-filter p? \t)))
                       (stream-filter p? \t))))))

(test
  |Leak test 5: Naive stream-filter should run in bounded space. Simplest case.|
  (is-false (eq 'stack-exhausted
                (until-stack-exhausted-or-timeout
                  (force (stream-filter (lambda (n) (= n 10000000000))
                                        (from 0)))))))

;========================================================================
; Leak test 6: Another long traversal should run in bounded space.

; The stream-ref procedure below does not strictly need to be lazy.
; It is defined lazy for the purpose of testing safe compostion of
; lazy procedures in the times3 benchmark below (previous
; candidate solutions had failed this).

(defun stream-ref (s index)
  (lazy
   (match (force s)
     (()      'error)
     ((h . \t) (if (zerop index)
                   (delay h)
                   (stream-ref \t (- index 1)))))))

(test |Check that evenness is correctly implemented - should terminate:|
  (is (= (force (stream-ref (stream-filter #'zerop (from 0))
                            0))
         0))
  (let (s)
    (setq s (stream-ref (from 0) 100000000))
    (is-false (eq 'stack-exhausted
                  (until-stack-exhausted-or-timeout
                    (force s))))))

;======================================================================
(defun times3 (n)
  (stream-ref (stream-filter
               (lambda (x) (zerop (mod x n)))
               (from 0))
              3))

(test |Leak test 7: Infamous example from SRFI 40.|
  (is (= 21
         (force (times3 7))))
  (is-false (eq 'stack-exhausted
                (until-stack-exhausted-or-timeout
                  (force (times3 100000000))))))

;;; eof
