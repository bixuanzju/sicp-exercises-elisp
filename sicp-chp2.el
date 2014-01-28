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

;; 2.7
(defun make-interval (a b)
  (cons a b))

(defun lower-bound (intvl)
  (car intvl))
(defun upper-bound (intvl)
  (cdr intvl))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; 2.8
(defun sub-interval (x y)
  (add-interval x
                (make-interval (- 0 (upper-bound y))
                               (- 0 (lower-bound y)))))

;; 2.9
(defun width (x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

;; (= (width (add-interval (make-interval 1 3) (make-interval 2 4)))
;;    (+ (width (make-interval 1 3)) (width (make-interval 2 4))))

;; (= (width (mul-interval (make-interval 1 3) (make-interval 2 4)))
;;    (* (width (make-interval 1 3)) (width (make-interval 2 4))))

;; 2.10
(defun div-interval-safe (x y)
  (if (= 0 (width y))
      (error "Divide by zero!")
    (mul-interval
     x
     (make-interval (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y))))))

;; 2.12
(defun make-center-percent (c per)
  (cons (- c (* per c)) (+ c (* per c))))
(defun center (x)
  (let ((start (lower-bound x))
        (end (upper-bound x)))
    (/ (float (- end start)) (+ end start))))


;; 2.13 sum of two tolerances

;; 2.17
(defun last-pair (lst)
  (if (= (length lst) 1)
      lst
    (last-pair (cdr lst))))

;; 2.18
(defun sicp-reverse (lst)
  (defun iter (l lst)
    (if (null l)
        lst
      (iter (cdr l) (cons (car l) lst))))
  (iter lst nil))

;; 2.19
(defun even? (a)
  (= (mod a 2) 0))
(defun odd? (a)
  (/= (mod a 2) 0))

(defun same-parity (a &rest lst)
  (defun trans (l p)
    (cond
     ((null l) nil)
     ((funcall p (car l)) (cons (car l) (trans (cdr l) p)))
     (t (trans (cdr l) p))))
  (if (even? a)
      (cons a (trans lst 'even?))
    (cons a (trans lst 'odd?))))

;; 2.21
(defun map (proc items)
  (if (null items)
      nil
    (cons (funcall proc (car items)) (map proc (cdr items)))))
(defun square-list1 (items)
  (if (null items)
      nil
    (cons (* (car items) (car items)) (square-list1 (cdr items)))))
(defun square-list2 (items)
  (map (lambda (k) (* k k)) items))

;; 2.23
(defun for-each (proc lst)
  (if (null lst) t
    (funcall proc (car lst))
    (for-each proc (cdr lst))))

;; 2.27
(defun deep-reverse (tree)
  (cond
   ((null tree) tree)
   ((atom (car tree)) (append (deep-reverse (cdr tree)) (list (car tree))))
   (t (append (deep-reverse (cdr tree))
              (list (deep-reverse (car tree)))))))

;; 2.28
(defun fringe (tree)
  (cond
   ((null tree) nil)
   ((atom (car tree)) (cons (car tree) (fringe (cdr tree))))
   (t (append (fringe (car tree))
              (fringe (cdr tree))))))

;; 2.29
(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun left-branch (m)
  (car m))
(defun right-branch (m)
  (car (cdr m)))
(defun branch-length (b)
  (car b))
(defun branch-structure (b)
  (car (cdr b)))

(defun total-weight (mobile)
  (let* ((left (left-branch mobile))
         (right (right-branch mobile))
         (ls (branch-structure left))
         (rs (branch-structure right)))
    (cond
     ((and (atom ls) (atom rs)) (+ ls rs))
     ((atom ls) (+ ls (total-weight rs)))
     ((atom rs) (+ (total-weight ls) rs))
     (t (+ (total-weight ls) (total-weight rs))))))

(defun balance? (mobile)
  (let* ((left (left-branch mobile))
         (right (right-branch mobile))
         (ls (branch-structure left))
         (ll (branch-length left))
         (rs (branch-structure right))
         (rl (branch-length right)))
    (cond
     ((and (atom ls) (atom rs)) (= (* ll ls) (* rl rs)))
     ((atom ls) (= (* rl (total-weight rs) (* ll ls))))
     ((atom rs) (= (* ll (total-weight ls) (* rl rs))))
     (t (= (* ll (total-weight ls)) (* rl (total-weight rs)))))))

;; 2.30
(defun square-tree1 (tree)
  (map (lambda (sub-tree)
         (if (atom sub-tree)
             (* sub-tree sub-tree)
           (square-tree sub-tree)))
       tree))
(defun square-tree2 (tree)
  (cond
   ((null tree) nil)
   ((atom (car tree)) (cons (* (car tree) (car tree))
                            (square-tree2 (cdr tree))))
   (t (cons (square-tree2 (car tree))
            (square-tree2 (cdr tree))))))

;; 2.31
(defun tree-map (proc tree)
  (map (lambda (sub-tree)
         (if (atom sub-tree)
             (funcall proc sub-tree)
           (tree-map proc sub-tree)))
       tree))

;; 2.32
(defun subset (s)
  (if (null s) (list nil)
    (let ((rest (subset (cdr s))))
      (append (map (lambda (sub) (cons (car s) sub)) rest) rest))))

;; 2.33
(defun filter (pred lst)
  (cond
   ((null lst) nil)
   ((funcall pred (car lst))
    (cons (car lst) (filter pred (cdr lst))))
   (t (filter pred (cdr lst)))))

(defun accumulate (op initial lst)
  (if (null lst) initial
    (funcall op (car lst) (accumulate op initial (cdr lst)))))

(defun sicp-map (p lst)
  (accumulate (lambda (x y) (cons (funcall p x) y)) nil lst))

(defun sicp-append (seq1 seq2)
  (accumulate 'cons seq2 seq1))

(defun sicp-length (lst)
  (accumulate (lambda (_ y) (1+ y)) 0 lst))

;; 2.34
(defun horner-eval (x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;; 2.35
(defun count-leaves1 (tr)
  (accumulate (lambda (tree leaves)
                (if (atom tree)
                    (+ 1 leaves)
                  (+ (count-leaves tree)
                     leaves)))
              0
              tr))
(defun count-leaves2 (tr)
  (accumulate '+ 0 (map (lambda (k)
                          (if (atom k) 1
                            (count-leaves2 k)))
                        tr)))

;; 2.36
(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
    (cons (accumulate op init (map 'car seqs))
          (accumulate-n op init (map 'cdr seqs)))))

;; 2.37
(defun map-extend (op &rest ls)
  (if (null (car ls))
      nil
    (cons (apply op (map 'car ls))
          (apply 'map-extend op (map 'cdr ls)))))

(defun dot-product (v w)
  (accumulate '+ 0 (map-extend '* v w)))

(defun matrix-*-vector (m v)
  (map (lambda (mv) (dot-product mv v)) m))

(defun transpose (mat)
  (accumulate-n 'cons nil mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;; 2.39
(defun fold-left (f acc l)
  (if (null l) acc
    (fold-left f (funcall f acc (car l)) (cdr l))))

(defun reverse1 (seq)
  (accumulate (lambda (x y) (append y (list x))) nil seq))
(defun reverse2 (seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

;; 2.40
(defun flatmap (proc seq)
  (accumulate 'append nil (map proc seq)))
(defun unique-pair (n)
  (flatmap (lambda (i) (map (lambda (j) (list j i))
                       (number-sequence 1 (- i 1))))
           (number-sequence 1 n)))

;; 2.41

(defun unique-triple (n)
  (filter (lambda (triple) (= (length triple)
                         (length (remove-duplicates triple))))
          (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                               (number-sequence 1 n)))
                                   (number-sequence 1 n)))
                   (number-sequence 1 n))))
(defun all-triple (n s)
  (filter (lambda (triple) (= s (apply '+ triple)))
          (unique-triple n)))

;; 2.42
(defun queens (board-size)
  (defun queen-cols (k)
    (if (= k 0)
        (list nil)
      (filter
       (lambda (positions) (safe? k positions))
       (flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position
                  new-row k rest-of-queens))
               (number-sequence 1 board-size)))
        (queen-cols (- k 1))))))
  (defun adjoin-position (row k q)
    (cons row q))
  (defun andall (lst)
    (if (null lst) t
      (and (car lst) (andall (cdr lst)))))
  (defun safe? (k pos)
    (let ((row (car pos))
          (col 1))
      (andall (map (lambda (pair) (and (/= row (car pair))
                                  (/= (abs (- (car pair) row))
                                      (abs (- (cdr pair) col)))))
                   (map-extend 'cons (cdr pos) (number-sequence 2 k))))))
  (queen-cols board-size))

;; 2.54
(defun equal? (a b)
  "Work for both number and symbol"
  (cond
   ((and (null a) (null b)) t)
   ((and (numberp a) (numberp b) (= a b)))
   ((and (symbolp a) (symbolp b)) (eq a b))
   ((and (listp a) (listp b))
    (and (equal? (car a) (car b))
         (equal? (cdr a) (cdr b))))
   (t nil)))

;; 2.56 2.57
(defun deriv (exp var)
  (cond
   ((numberp exp) 0)
   ((variable? exp) (if (same-variable? exp var) 1 0))
   ((sum? exp) (make-sum (deriv (addend exp) var)
                         (deriv (augend exp) var)))
   ((product? exp)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (multiplicand exp)
                   (deriv (multiplier exp) var))))
   ((exponentiation? exp)
    (make-product
     (make-product (exponent exp)
                   (make-exponentiation (base exp)
                                        (- (exponent exp) 1)))
     (deriv (base exp) var)))
   (t (error "unknown expression type: DERIV"))))


(defun variable? (exp) (symbolp exp))

(defun same-variable? (exp1 exp2)
  (and (variable? exp1) (variable? exp2) (eq exp1 exp2)))

(defun make-sum (a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   ((and (numberp a1) (numberp a2)) (+ a1 a2))
   (t (list '+ a1 a2))))

(defun =number? (exp n) (and (numberp exp) (= exp n)))

(defun make-product (m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (numberp m1) (numberp m2)) (* m1 m2))
   (t (list '* m1 m2))))

(defun sum? (exp) (and (listp exp) (eq (car exp) '+)))

(defun addend (s) (cadr s))

(defun augend (s)
  (if (= (length (cddr s)) 1)
      (caddr s)
    (cons '+ (cddr s))))

(defun product? (exp) (and (listp exp) (eq (car exp) '*)))

(defun multiplier (p) (cadr p))

(defun multiplicand (p)
  (if (= (length (cddr p)) 1)
      (caddr p)
    (cons '* (cddr p))))

(defun exponentiation? (exp) (and (listp exp) (eq (car exp) '**)))

(defun base (exp) (cadr exp))

(defun exponent (exp) (caddr exp))

(defun make-exponentiation (base exp)
  (cond
   ((= exp 1) base)
   ((= exp 0) 1)
   (t (list '** base exp))))

;; 2.59
(defun element-of? (x set)
  (cond
   ((null set) nil)
   ((equal? x (car set)) t)
   (t (element-of? x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of? x set) set
    (cons x set)))

(defun intersection-set (s1 s2)
  (cond
   ((or (null s1) (null s2)) nil)
   ((element-of? (car s1) s2)
    (cons (car s1) (intersection-set (cdr s1) s2)))
   (t (intersection-set (cdr s1) s2))))

(defun union-set (s1 s2)
  (cond
   ((null s1) s2)
   ((null s2) s1)
   ((element-of? (car s1) s2)
    (union-set (cdr s1) s2))
   (t (cons (car s1) (union-set (cdr s1) s2)))))

;; 2.60
(defun element-of2? (x set)
  (cond
   ((null set) nil)
   ((= x (car set)) t)
   ((< x (car set)) nil)
   (t (element-of2? x (cdr set)))))

(defun intersection-set2 (s1 s2)
  (let ((x1 (car s1))
        (x2 (car s2)))
    (cond
     ((or (null s1) (null s2)) nil)
     ((= x1 x2)
      (cons x1 (intersection-set2 (cdr s1) (cdr s2))))
     ((< x1 x2)
      (intersection-set2 (cdr s1) s2))
     (t (intersection-set2 s1 (cdr s2))))))

(defun adjoin-set2 (x set)
  (cond
   ((null set) (list x))
   ((= x (car set)) set)
   ((< x (car set)) (cons x set))
   (t (cons (car set) (adjoin-set2 x (cdr set))))))

(defun union-set2 (s1 s2)
  (let ((x1 (car s1))
        (x2 (car s2)))
    (cond
     ((null s1) s2)
     ((null s2) s1)
     ((< x1 x2)
      (cons x1 (union-set2 (cdr s1) s2)))
     ((> x1 x2)
      (cons x2 (union-set2 s1 (cdr s2))))
     (t (cons x1 (union-set2 (cdr s1) (cdr s2)))))))

;; 2.61 2.62
(defun entry (tree) (car tree))

(defun branch-left (tree) (cadr tree))

(defun branch-right (tree) (caddr tree))

(defun make-tree (entry left right) (list entry left right))

(defun element-of3? (x set)
  (cond
   ((null set) nil)
   ((= x (entry set)) t)
   ((< x (entry set)) (element-of3? x (branch-left set)))
   (t (element-of3? x (branch-right set)))))

(defun adjoin-set3 (x set)
  (cond
   ((null set) (make-tree x nil nil))
   ((= x (entry set)) set)
   ((< x (entry set))
    (make-tree
     (entry set)
     (adjoin-set3 x (branch-left set))
     (branch-right set)))
   (t (make-tree
       (entry set)
       (branch-left set)
       (adjoin-set3 x (branch-right set))))))

;; 2.63 same result, tree->list2 more efficient
(defun tree->list1 (tree)
  (if (null tree) nil
    (append (tree->list1 (branch-left tree))
            (cons (entry tree)
                  (tree->list1 (branch-right tree))))))

(defun tree->list2 (tree)
  (defun copy-to-list (tree result-list)
    (if (null tree) result-list
      (copy-to-list (branch-left tree)
                    (cons (entry tree)
                          (copy-to-list
                           (branch-right tree)
                           result-list)))))
  (copy-to-list tree nil))

;; 2.64
(defun list->tree (elements)
  (car (partial-tree elements (length elements))))
(defun partial-tree (elts n)
  (if (= n 0)
      (cons nil elts)
    (let* ((left-size (/ (- n 1) 2))
           (left-result
            (partial-tree elts left-size))
           (left-tree (car left-result))
           (non-left-elts (cdr left-result))
           (right-size (- n (+ 1 left-size)))
           (this-entry (car non-left-elts))
           (right-result
            (partial-tree
             (cdr non-left-elts)
             right-size))
           (right-tree (car right-result))
           (remaining-elts (cdr right-result)))
      (cons (make-tree this-entry
                       left-tree
                       right-tree)
            remaining-elts))))

;; 2.65
(defun union-set3 (s1 s2)
  (let* ((lst1 (tree->list2 s1))
         (lst2 (tree->list2 s2)))
    (list->tree (union-set2 lst1 lst2))))

(defun intersection-set3 (s1 s2)
  (let* ((lst1 (tree->list2 s1))
         (lst2 (tree->list2 s2)))
    (list->tree (intersection-set2 lst1 lst2))))

;; 2.67
(defun make-leaf (symbol weight) (list 'leaf symbol weight))

(defun leaf? (object) (eq (car object) 'leaf))

(defun symbol-leaf (x) (cadr x))

(defun weight-leaf (x) (caddr x))

(defun make-code-tree (left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defun hft-left (tree) (car tree))

(defun hft-right (tree) (cadr tree))

(defun symbols (tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
      (caddr tree)))

(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
    (cadddr tree)))

(defun decode (bits tree)
  (defun decode-1 (bits current-branch)
    (if (null bits)
        nil
      (let ((next-branch
             (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(defun choose-branch (bit branch)
  (cond
   ((= bit 1) (hft-right branch))
   ((= bit 0) (hft-left branch))
   (t (error "bad bit"))))

(defun hft-adjoin-set (x set)
  (cond
   ((null set) (list x))
   ((< (weight x) (weight (car set))) (cons x set))
   (t (cons (car set) (hft-adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      nil
    (let ((pair (car pairs)))
      (hft-adjoin-set (make-leaf (car pair)
                                 (cadr pair))
                      (make-leaf-set (cdr pairs))))))
(setq sample-tree
      (make-code-tree (make-leaf 'A 4)
                      (make-code-tree
                       (make-leaf 'B 2)
                       (make-code-tree
                        (make-leaf 'D 1)
                        (make-leaf 'C 1)))))
(setq sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; 2.68
(defun encode (message tree)
  (if (null message)
      nil
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(defun encode-symbol (sym tree)
  (if (memq sym (symbols tree))
      (if (leaf? tree)
          nil
        (let* ((left-branch (hft-left tree))
               (left-symbols (symbols left-branch))
               (right-branch (hft-right tree))
               (right-symbols (symbols right-branch)))
          (cond
           ((memq sym left-symbols)
            (cons 0 (encode-symbol sym left-branch)))
           ((memq sym right-symbols)
            (cons 1 (encode-symbol sym right-branch))))))
    (error "Symbol not in the tree")))

;; 2.69
(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

(defun successive-merge (set)
  (if (= 1 (length set))
      (car set)
    (let ((new-node (make-code-tree (car set) (cadr set))))
      (successive-merge (hft-adjoin-set new-node (cddr set))))))

;; 2.70
;; (setq lyric-tree (generate-huffman-tree '((A 2) (GET 2) (SHA 3)
;;                                         (WAH 1) (BOOM 1) (JOB 1)
;;                                         (NA 16) (YIP 9))))
;; (length (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB
;;                       SHA NA NA NA NA NA NA NA NA
;;                       WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
;;                       SHA BOOM) lyric-tree))



;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
