;;; -*- lexical-binding: t -*-

;; 2.1
(defun make-rat (n d)
  (defun gcd (a b)
    (cond
     ((> b a) (gcd b a))
     ((= b 0) a)
     (t (gcd b (% a b)))))
  (let* ((pos-n (abs n))
         (pos-d (abs d))
         (g (gcd pos-n pos-d))
         (sign (* n d)))
    (if (< sign 0)
        (cons (- 0 (/ pos-n g)) (/ pos-d g))
      (cons (/ pos-n g) (/ pos-d g)))))

;; 2.2
(defun make-segment (p1 p2)
  (cons p1 p2))

(defun start-segment (l)
  (car l))

(defun end-segment (l)
  (cdr l))

(defun make-point (x y)
  (cons x y))

(defun x-point (p)
  (car p))

(defun y-point (p)
  (cdr p))

(defun midpoint-segment (l)
  (let ((start (start-segment l))
        (end (end-segment l)))
    (make-point (/ (+ (x-point start)
                      (x-point end))
                   2.0)
                (/ (+ (y-point start)
                      (y-point end))
                   2.0))))

(defun print-point (p)
  (message "(%f, %f)" (x-point p) (y-point p)))

;; 2.4
(defun sicp-cons (x y)
  (lambda (m) (funcall m x y)))
(defun sicp-car (z)
  (funcall z (lambda (p _) p)))
(defun sicp-cdr (z)
  (funcall z (lambda (_ q) q)))

;; 2.5
(defun cons2 (a b)
  (* (expt 2 a) (expt 3 b)))
(defun car2 (n)
  (many n 2))
(defun cdr2 (n)
  (many n 3))

(defun many (n base)
  (defun iter (n k)
    (if (= (% n base) 0)
        (iter (/ n base) (1+ k))
      k))
  (iter n 0))

;; 2.6
(defconst church-zero (lambda (_) (lambda (x) x)))
(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))
(defconst church-one (lambda (f) (lambda (x) (funcall f x))))
(defconst church-two (lambda (f) (lambda (x) (funcall f (funcall f x)))))

(defun plus (n m)
  "Verify by eval `(funcall (funcall (plus church-one church-two) '1+) 0)',
   which should return 3"
  (lambda (f) (lambda (x) (funcall (funcall n f) (funcall (funcall m f) x)))))



;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
