;;; sicp-chp1.el --- exercises from chp1 -*- lexical-binding: t -*-

;; Copyright (C) Jeremy Bi

;; Author: Jeremy Bi <xbi@zju.edu.cn>
;; Maintainer: Jeremy Bi <xbi@zju.edu.cn>
;; Created:   1 Feb 2014
;; Keywords: convenience editing
;; URL: https://github.com/bixuanzju/emacs_repo

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;; 1.11
(defun f-recr (n)
  "By means of a recursive process"
  (if (< n 3)
      n
    (+ (f-recr (- n 1)) (* 2 (f-recr (- n 2))) (* 3 (f-recr (- n 3))))))
(defun f-iter (n)
  "By means of a iterative process"
  (defun help (a1 a2 a3 count)
    (if (= count 0) a3
      (help (+ a1 (* 2 a2) (* 3 a3)) a1 a2 (- count 1))))
  (help 2 1 0 n))

;; 1.12
(defun pascal-triangle (a b)
  "Computer binomial coefficient"
  (cond
   ((= 0 b) 1)
   ((= a b) 1)
   (t (+ (pascal-triangle (- a 1) (- b 1)) (pascal-triangle (- a 1) b)))))

;; 1.16
(defun even? (a)
  (= (% a 2) 0))

(defun fast-exp-iter (b n)
  "Use invariant quantity a*base^count,this is
a powerful technique when designing iterative algorithms."
  (cl-labels
      ((iter (a base count)
             (cond
              ((= count 0) a)
              ((even? count) (iter a (* base base) (/ count 2)))
              (t (iter (* a base) base (- count 1))))))
    (iter 1 b n)))

;; 1.17
(defun double (n)
  (+ n n))
(defun halve (n)
  (/ n 2))

(defun log-multi (a b)
  (cond
   ((= b 0) 0)
   ((even? b) (double (log-multi a (halve b))))
   (t (+ a (log-multi a (- b 1))))))

;; 1.18
(defun log-multi-iter (a b)
  "Use invariant quantity s+ab"
  (cl-labels
      ((iter (s a b)
             (cond
              ((= b 0) s)
              ((even? b) (iter s (double a) (halve b)))
              (t (iter (+ s a) a (- b 1))))))
    (iter 0 a b)))

;; 1.19
(defun fib (n)
  (fib-iter 1 0 0 1 n))
(defun fib-iter (a b p q count)
  (cond
   ((= count 0) b)
   ((even? count)
    (fib-iter a
              b
              (+ (* p p) (* q q))
              (+ (* q q) (* 2 p q))
              (/ count 2)))
   (t (fib-iter (+ (* b q) (* a q) (* a p))
                (+ (* b p) (* a q))
                p
                q
                (- count 1)))))

;; 1.23
(defun smallest-divisor (n)
  (find-divisor n 2))
(defun find-divisor (n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (t (find-divisor n (next test-divisor)))))
(defun square (n)
  (* n n))
(defun divides? (a b)
  (= (mod b a) 0))
(defun next (n)
  (if (= 2 n) 3
    (+ 2 n)))
(defun prime? (n)
  (cond
   ((= n 1) nil)
   (t (= (smallest-divisor n) n))))

;; 1.29
(defun sum (term a next b)
  (if (> a b) 0
    (+ (funcall term a) (sum term (funcall next a) next b))))

(defun integral (f a b n)
  (let* ((h (/ (- b a) (float n)))
         (term (lambda (k) (if (even? k)
                          (* 2 (funcall f (+ a (* k h))))
                        (* 4 (funcall f (+ a (* k h))))))))
    (* (/ h 3) (+ (funcall f a)
                  (funcall f (+ a (* n h)))
                  (sum term 1 '1+ (- n 1))))))

(defun cube (n) (* n n n))

;; 1.30
(defun sum-iter (term a next b)
  (cl-labelss
   ((iter (a result)
          (if (> a b) result
            (iter (funcall next a) (+ result (funcall term a))))))
   (iter a 0)))

;; 1.31
(defun product (term a next b)
  (if (> a b) 1
    (* (funcall term a) (product term (funcall next a) next b))))

(defun factorial (n)
  (product (lambda (x) x) 1 '1+ n))

(defun pi-compute (n)
  (let ((f (lambda (k) (/ (* k k) (float (* (- k 1) (+ k 1)))))))
    (* (/ 8 (float 3)) (product f 4 (lambda (n) (+ n 2)) (* n 2)))))

(defun product-iter (term a next b)
  (cl-labels
      ((iter (a result)
             (if (> a b) result
               (iter (funcall next a) (* result (funcall term a))))))
    (iter a 1)))

;; 1.32
(defun accumulate (combiner null-value term a next b)
  (if (> a b) null-value
    (funcall combiner
             (funcall term a)
             (accumulate combiner null-value term (funcall next a) next b))))

(defun sum-c (term a next b)
  (accumulate-iter '+ 0 term a next b))

(defun product-c (term a next b)
  (accumulate '* 1 term a next b))

(defun accumulate-iter (combiner null-value term a next b)
  (cl-labels
      ((iter (a result)
             (if (> a b) result
               (iter (funcall next a) (funcall combiner result (funcall term a))))))
    (iter a null-value)))

(defun fold-l (f acc l)
  (if (null l) acc
    (fold-l f (funcall f acc (car l)) (cdr l))))

(defun fold-r (f acc l)
  (if (null l) acc
    (funcall f (car l) (fold-r f acc (cdr l)))))

;; 1.33
(defun filtered-accumulate (combiner null-value term a next b pred)
  (cond
   ((> a b) null-value)
   ((funcall pred a)
    (funcall combiner
             (funcall term a)
             (filtered-accumulate combiner null-value term (funcall next a)
                                  next b pred)))
   (t (filtered-accumulate combiner null-value term (funcall next a)
                           next b pred))))

(defun sum-square-prime (a b)
  (filtered-accumulate '+ 0 (lambda (k) (* k k)) a '1+ b 'prime?))

(defun gcd (a b)
  (cond
   ((> b a) (gcd b a))
   ((= b 0) a)
   (t (gcd b (% a b)))))

(defun product-of-prime (n)
  (filtered-accumulate '* 1 (lambda (k) k) 1 '1+ (- n 1) (lambda (k) (= (gcd k n) 1))))

;; 1.35
(defconst sicp-tolerance 0.00001)

(defun fixed-point (f first-guess)
  (cl-labels
      ((close-enough? (v1 v2)
                      (< (abs (- v1 v2))
                         sicp-tolerance))
       (try (guess)
            (let ((next (funcall f guess)))
              (if (close-enough? next guess)
                  next
                (try next)))))
    (try first-guess)))

(defconst golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; 1.36
(defun fixed-point-print (f first-guess)
  (cl-labels
      ((close-enough? (v1 v2)
                      (< (abs (- v1 v2))
                         sicp-tolerance))
       (try (guess)
            (let ((next (funcall f guess)))
              (if (close-enough? next guess)
                  next
                (message "%s" next)
                (try next)))))
    (try first-guess)))

;; (fixed-point-print (lambda (x) (/ (log 1000) (log x))) 5.0)

;; (fixed-point-print (lambda (x) (* 0.5 (+ x (/ (log 1000) (log x))))) 5.0)

;; 1.37
(defun cont-frac (n d k)
  (cl-labels
      ((iter (kk result)
             (if (= kk 0) result
               (iter (- kk 1) (/ (funcall n kk) (+ (funcall d kk) result))))))
    (iter k 0)))


;; 1.38
(defun di (k)
  (if (= (% (+ k 1) 3) 0)
      (* (/ (+ k 1) 3) 2)
    1))

(defconst sicp-e (+ 2 (cont-frac (lambda (_) 1.0) 'di 400)))

;; 1.39
(defun tan-cf (x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                 (* -1 x x)))
             (lambda (i) (- (* 2 i) 1))
             k))

;; 1.40
(defconst sicp-dx 0.00001)
(defun deriv (g)
  (lambda (x) (/ (- (funcall g (+ x sicp-dx)) (funcall g x)) sicp-dx)))

(defun newton-transformer (g)
  (lambda (x) (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newton-method (g guess)
  (fixed-point (newton-transformer g) guess))

(defun cubic (a b c)
  (lambda (x) (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

(defun find-zero (a b c)
  (newton-method (cubic a b c) 1.0))

;; 1.41
(defun double-func (g)
  (lambda (x) (funcall g (funcall g x))))

;; 1.42
(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;; 1.43
(defun repeated (f n)
  (if (= n 1) f
    (compose f (repeated f (- n 1)))))

;; 1.44
(defun smooth (f)
  (lambda (x) (/ (+ (funcall f (- x sicp-dx)) (funcall f x) (funcall f (+ x sicp-dx)))
            3)))
(defun n-fold-smooth (f n)
  (funcall (repeated 'smooth n) f))

;; 1.45
(defun average-damp (f)
  (lambda (x) (/ (+ (funcall f x) x) 2)))

(defun nth-root (x n)
  (fixed-point (funcall (repeated 'average-damp (- n 2))
                        (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;; 1.46
(defun iterative-improve (good-enough? improve)
  (cl-labels
      ((iter (guess)
             (if (funcall good-enough? guess)
                 guess
               (iter (funcall improve guess)))))
    (lambda (guess)
      (iter guess))))

(defun sqrt-iter (x)
  (cl-labels
      ((good-enough? (guess)
                     (< (abs (- (square guess) x)) sicp-tolerance))
       (improve (guess)
                (funcall (average-damp (lambda (y) (/ x y))) guess)))
    (funcall (iterative-improve 'good-enough? 'improve) 1.0)))

(defun fixed-point-iter (f first-guess)
  (cl-labels
      ((close-enough? (guess)
                      (< (abs (- (funcall f guess) guess)) sicp-tolerance))
       (improve (guess)
                (funcall f guess)))
    (funcall (iterative-improve 'close-enough? 'improve) first-guess)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; sicp-chp1.el ends here
