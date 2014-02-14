;;; sicp-chp3.el --- exercises from chp3 -*- lexical-binding: t -*-

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

;; 3.1
(defun make-accumulator (init)
  (lambda (n)
    (progn
      (setq init (+ init n))
      init)))
;; (fset 'A (make-accumulator 5))
;; (A 10)
;; (A 10)

;; 3.2
(defun make-monitor (f)
  (let ((count 0))
    (defun mf (m)
      (cond
       ((eq m 'how-many-calls?)
        count)
       ((eq m 'reset-count)
        (setq count 0))
       (t (progn
            (setq count (1+ count))
            (funcall f m)))))
    #'mf))
;; (fset 's (make-monitor 'sqrt))
;; (s 100)
;; (s 'how-many-calls?)
;; (s 'reset-count)

;; 3.3
(defun make-account (balance pwd)
  (defun withdraw (amount)
    (if (>= balance amount)
        (progn
          (setq balance (- balance amount))
          balance)
      "Insufficient funds"))
  (defun deposit (amount)
    (setq balance (+ balance amount)))
  (lambda (secret m)
    (if (eq secret pwd)
        (cond ((eq m 'withdraw) #'withdraw)
              ((eq m 'deposit) #'deposit)
              (t (error "Unknown request")))
      (lambda (_) "Incorrect password"))))

;; (fset 'acc (make-account 100 'secret-password))
;; (funcall (acc 'secret-password 'withdraw) 40)
;; (funcall (acc 'password 'deposit) 40)

;; 3.4
(defun make-account2 (balance pwd)
  (let ((time 0))
    (defun withdraw (amount)
      (if (>= balance amount)
          (progn
            (setq balance (- balance amount))
            balance)
        "Insufficient funds"))
    (defun deposit (amount)
      (setq balance (+ balance amount)))
    (lambda (secret m)
      (if (eq secret pwd)
          (cond ((eq m 'withdraw) (setq time 0) #'withdraw)
                ((eq m 'deposit) (setq time 0) #'deposit)
                (t (setq time 0) (error "Unknown request")))
        (lambda (_)
          (if (= time 7)
              (progn
                (setq time 0)
                (error "I am calling the police"))
            (progn
              (setq time (1+ time))
              "Incorrect password")))))))

;; (fset 'acc (make-account2 100 'secret-password))
;; (funcall (acc 'secret-password 'withdraw) 40)
;; (funcall (acc '-password 'deposit) 40)

;; 3.5
(defun estimate-integral (pred x1 x2 y1 y2 trials)
  (defun iter (trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((funcall pred x1 x2 y1 y2)
           (iter (- trials-remaining 1)
                 (1+ trials-passed)))
          (t (iter (- trials-remaining 1)
                   trials-passed))))
  (* (- x2 x1) (- y2 y1) (iter trials 0.0)))

(defun pi-predicate (x1 x2 y1 y2)
  (let ((xc (/ (- x2 x1) 2))
        (yc (/ (- y2 y1) 2))
        (rx (random-in-range x1 x2))
        (ry (random-in-range y1 y2)))
    (>= 1 (+ (expt (- rx xc) 2) (expt (- ry yc) 2)))))

(defun random-in-range (low high)
  (let ((range (- high low)))
    (+ low (random* range))))

(defun estimate-pi (trials)
  (estimate-integral 'pi-predicate 0.0 2.0 0.0 2.0 trials))

;; 3.6
(defun rand-new (m)
  (cond ((eq m 'generate) (random))
        ((eq m 'reset)
         (lambda (value)
           (random value)))))
;; (rand-new 'generate)
;; (funcall (rand-new 'reset) "hah")

;; 3.7
(defun make-account3 (balance &rest pwds)
  (defun withdraw (amount)
    (if (>= balance amount)
        (progn
          (setq balance (- balance amount))
          balance)
      "Insufficient funds"))
  (defun deposit (amount)
    (setq balance (+ balance amount)))
  (defun add-pwd (npwd)
    (add-to-list 'pwds npwd))
  (lambda (secret m)
    (if (memq secret pwds)
        (cond ((eq m 'withdraw) #'withdraw)
              ((eq m 'deposit) #'deposit)
              ((eq m 'add-pwd) #'add-pwd)
              (t (error "Unknown request")))
      (lambda (_) "Incorrect password"))))

;; (setq peter-acc (make-account3 100 'open-sesame))
;; (funcall (funcall peter-acc 'open-sesame 'deposit) 30)

(defun make-joint (account pwd npwd)
  (progn
    (if (listp (funcall (funcall account pwd 'add-pwd) npwd))
        account
      (error "Incorrent password"))))

;; (setq paul-acc
;;       (make-joint peter-acc 'open-sesame 'rousebud))
;; (funcall (funcall peter-acc 'ronusebud 'withdraw) 30)

(defun last-pair (x)
  (if (null (cdr x)) x (last-pair (cdr x))))

(defun make-cycle (x)
  (setcdr (last-pair x) x)
  x)

;; 3.14
(defun mystery (x)
  (defun myloop (x y)
    (if (null x)
        y
      (let ((temp (cdr x)))
        (setcdr x y)
        (myloop temp x))))
  (myloop x '()))


;; 3.16
(defun count-pair (x)
  (if (not (consp x))
      0
    (+ (count-pair (car x))
       (count-pair (cdr x))
       1)))

;; 3.17
(defun count-pair-c (x)
  (let ((temp '()))
    (defun iter (xs)
      (cond
       ((not (consp xs)) 0)
       ((memq xs temp) 0)
       (t (setq temp (cons xs temp))
          (+ 1
             (iter (car xs))
             (iter (cdr xs))))))
    (iter x)))

;; (setq a '(1))
;; (setq b (cons a a))
;; (setq c (cons b b))

;; 3.18
(defun detect-cycle (x)
  (let ((temp '()))
    (defun iter (xs)
      (cond
       ((null xs) nil)
       ((memq xs temp) t)
       (t (setq temp (cons xs temp))
          (iter (cdr xs)))))
    (iter x)))

(setq a (make-cycle '(1 2 3 4)))

;; 3.19
(defun detect-cycle-clever (x)
  (defun iter (a b)
    (cond
     ((eq a b) t)
     ((or (null a) (null b)) nil)
     (t (iter (cdr-safe a) (cdr-safe (cdr-safe b))))))
  (iter x (cdr-safe x)))


;; 3.21
(defun front-ptr (queue)
  (car queue))

(defun rear-ptr (queue)
  (cdr queue))

(defun set-front-ptr! (queue item)
  (setcar queue item))

(defun set-rear-ptr! (queue item)
  (setcdr queue item))

(defun empty-queue? (queue)
  (null (front-ptr queue)))

(defun make-queue ()
  (cons '() '()))

(defun front-queue (queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue")
    (car (front-ptr queue))))

(defun insert-queue! (queue item)
  (let ((new-pair (cons item '())))
    (cond
     ((empty-queue? queue)
      (set-front-ptr! queue new-pair)
      (set-rear-ptr! queue new-pair)
      queue)
     (t
      (setcdr (rear-ptr queue) new-pair)
      (set-rear-ptr! queue new-pair)
      queue))))

(defun delete-queue! (queue)
  (cond
   ((empty-queue? queue)
    (error "DELETE called with an empty queue"))
   (t
    (set-front-ptr! queue (cdr (front-ptr queue)))
    queue)))

(defun print-queue (queue)
  (front-ptr queue))

(setq q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))

;; 3.22
(defun make-queue2 ()
  (let ((front-ptr '())
        (rear-ptr '()))
    (defun empty-queue? ()
      (null front-ptr))
    (defun front-queue ()
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
        (car front-ptr)))
    (defun insert-queue! (item)
      (let ((new-pair (cons item '())))
        (cond
         ((empty-queue?)
          (setq front-ptr new-pair)
          (setq rear-ptr new-pair))
         (t (setcdr rear-ptr new-pair)
            (setq rear-ptr new-pair)))))
    (defun delete-queue! ()
      (cond
       ((empty-queue?)
        (error "DELETE called with an empty queue"))
       (t (setq front-ptr (cdr front-ptr)))))
    (lambda (m)
      (cond
       ((eq m 'make-queue) #'make-queue)
       ((eq m 'empty-queue?) #'empty-queue?)
       ((eq m 'front-queue) #'front-queue)
       ((eq m 'insert-queue!) #'insert-queue!)
       ((eq m 'delete-queue!) #'delete-queue!)
       (t (error "Unknown command"))))))

(defun empty-queue2? (queue)
  (funcall (funcall queue 'empty-queue?)))

(defun front-queue2 (queue)
  (funcall (funcall queue 'front-queue)))

(defun insert-queue2! (queue item)
  (funcall (funcall queue 'insert-queue!) item))

(defun delete-queue2! (queue)
  (funcall (funcall queue 'delete-queue!)))

;; (setq q1 (make-queue2))
;; (insert-queue2! q1 'a)
;; (insert-queue2! q1 'b)
;; (front-queue2 q1)
;; (delete-queue2! q1)


;; 3.23: use double-linked list
(defun front-ptr-de (dq)
  (car dq))
(defun rear-ptr-de (dq)
  (cdr dq))
(defun set-front-ptr-de! (dq item)
  (setcar dq item))
(defun set-rear-ptr-de! (dq item)
  (setcdr dq item))
(defun empty-deque? (dq)
  (null (front-ptr-de dq)))
(defun make-deque ()
  (cons '() '()))
(defun front-deque (dq)
  (if (empty-deque? dq)
      (error "FRONT called with an empty deque")
    (caar (front-ptr-de dq))))
(defun rear-deque (dq)
  (if (empty-deque? dq)
      (error "REAR called with an empty deque")
    (caar (rear-ptr-de dq))))
(defun front-insert-deque! (dq item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond
     ((empty-deque? dq)
      (set-front-ptr-de! dq new-pair)
      (set-rear-ptr-de! dq new-pair)
      dq)
     (t
      (setcdr new-pair (front-ptr-de dq))
      (setcdr (car (front-ptr-de dq)) new-pair)
      (set-front-ptr-de! dq new-pair)
      dq))))
(defun rear-insert-deque! (dq item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond
     ((empty-deque? dq)
      (set-front-ptr-de! dq new-pair)
      (set-rear-ptr-de! dq new-pair)
      dq)
     (t
      (setcdr (rear-ptr-de dq) new-pair)
      (setcdr (car new-pair) (rear-ptr-de dq))
      (set-rear-ptr-de! dq new-pair)
      dq))))
(defun front-delete-deque! (dq)
  (cond
   ((empty-deque? dq)
    (error "DELETE called with an empty deque"))
   ((eq (front-ptr-de dq) (rear-ptr-de dq))
    (setcar dq nil)
    (setcdr dq nil)
    dq)
   (t
    (set-front-ptr-de! dq (cdr (front-ptr-de dq)))
    (setcdr (car (front-ptr-de dq)) nil)
    dq)))
(defun rear-delete-deque! (dq)
  (cond
   ((empty-deque? dq)
    (error "DELETE called with an empty deque"))
   ((eq (front-ptr-de dq) (rear-ptr-de dq))
    (setcar dq nil)
    (setcdr dq nil)
    dq)
   (t
    (set-rear-ptr-de! dq (cdar (rear-ptr-de dq)))
    (setcdr (rear-ptr-de dq) nil)
    dq)))
(defun print-deque (dq)
  (defun iter (ptr lst)
    (if (null ptr) (reverse lst)
      (iter (cdr ptr) (cons (caar ptr) lst))))
  (iter (front-ptr-de dq) '()))

;; (setq dq1 (make-deque))
;; (front-insert-deque! dq1 'a)
;; (rear-insert-deque! dq1 'b)
;; (rear-insert-deque! dq1 'c)
;; (print-deque dq1)
;; (front-deque dq1)
;; (rear-delete-deque! dq1)
;; (front-delete-deque! dq1)



(defun assoc (key records)
  (cond
   ((null records) nil)
   ((equal (caar records) key) (car records))
   (t (assoc key (cdr records)))))

(defun make-table ()
  (cons 'table nil))

;; 3.24
(defun make-table-local (same-key?)
  (let ((local-table (list 'table)))
    (defun assoc (key records)
      (cond
       ((null records) nil)
       ((funcall same-key? (caar records) key) (car records))
       (t (assoc key (cdr records)))))
    (defun lookup (key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record (cdr record) nil))
          nil)))
    (defun insert! (key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (setcdr record value)
                (setcdr subtable
                        (cons (cons key2 value) (cdr subtable)))))
          (setcdr local-table
                  (cons (list key1 (cons key2 value))
                        (cdr local-table)))))
      'ok)
    (lambda (m)
      (cond
       ((eq m 'lookup-proc) #'lookup)
       ((eq m 'insert-proc!) #'insert!)
       (t (error "Unknown operation"))))))

;; 3.25
(defun foldl (op acc lst)
  (cond
   ((null lst) acc)
   (t (foldl op (funcall op acc (car lst)) (cdr lst)))))

(defun make-table-g ()
  (let ((local-table (list '*table*)))
    (defun lookup (key-lst)
      (foldl
       (lambda (records key)
         (cond
          ((null records) nil)
          ((consp records)
           (let ((record (assoc key records)))
             (if record (cdr record) nil)))
          (t nil)))
       (cdr local-table)
       key-lst))
    (defun iter (subtable keys)
      (cond
       ((null keys)
        (cons subtable nil))
       ((consp (cdr subtable))
        (cons subtable keys))
       (t (let ((record (assoc (car keys) (cdr subtable))))
            (if record
                (iter record (cdr keys))
              (cons subtable keys))))))
    (defun make-new (keys value)
      (if (null (cdr keys))
          (cons (car keys) value)
        (list (car keys) (make-new (cdr keys) value))))
    (defun insert! (key-lst value)
      (let ((last-table (car (iter local-table key-lst)))
            (last-keys (cdr (iter local-table key-lst))))
        (if (null last-keys)
            (setcdr last-table value)
          (setcdr last-table (cons (make-new last-keys value)
                                   (cdr last-table)))))
      'ok)
    (lambda (m)
      (cond
       ((eq m 'lookup-proc) #'lookup)
       ((eq m 'insert-proc!) #'insert!)
       (t (error "Unknown operation"))))))

;; (setq t2 (make-table-g))
;; (setq get-value (funcall t2 'lookup-proc))
;; (setq put-value (funcall t2 'insert-proc!))

;; (funcall put-value '(1 2 3) 4)
;; (funcall put-value '(1 2 5 6) 7)
;; (funcall put-value '(1 2 5) 0)
;; (funcall get-value '(1 2 5 6))

;; 3.26
(defun make-table-b ()
  (let ((local-table))
    (defun entry (tree) (car tree))
    (defun branch-left (tree) (cadr tree))
    (defun branch-right (tree) (caddr tree))
    (defun make-tree (entry left right) (list entry left right))
    (defun lookup (key)
      (defun lookup-iter (table)
        (cond ((null table)
               nil)
              ((= (car (entry table))
                  key)
               (cdr (entry table)))
              ((> (car (entry table))
                  key)
               (lookup-iter (branch-right table)))
              (t (lookup-iter (branch-left table)))))
      (lookup-iter local-table))
    (defun insert! (key value)
      (defun insert-iter (table)
        (cond
         ((null table)
          (make-tree (cons key value) nil nil))
         ((= (car (entry table)) key)
          (setcdr (entry table) value)
          table)
         ((> (car (entry table)) key)
          (make-tree (entry table)
                     (branch-left table)
                     (insert-iter (branch-right table))))
         (t (make-tree
             (entry table)
             (insert-iter (branch-left table))
             (branch-right table)))))
      (setq local-table (insert-iter local-table))
      'ok)
    (lambda (m)
      (cond
       ((eq m 'lookup-proc) #'lookup)
       ((eq m 'insert-proc!) #'insert!)
       (t (error "Unknown operation"))))))

;; (setq t1 (make-table-b))
;; (setq t2 (make-table-b))
;; (setq get-value (funcall t1 'lookup-proc))
;; (setq set-value (funcall t1 'insert-proc!))
;; (funcall set-value 2 4)
;; (funcall set-value 7 9)
;; (funcall set-value 1 5)
;; (funcall get-value 1)


(defun inverter (input output)
  (defun invert-input ()
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input #'invert-input)
  'ok)
(defun logical-not (s)
  (cond
   ((= s 0) 1)
   ((= s 1) 0)
   (t (error "Invalid signal"))))

(defun and-gate (a1 a2 output)
  (defun add-action-procedure ()
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 #'add-action-procedure)
  (add-action! a2 #'add-action-procedure)
  'ok)
(defun logical-and (a1 a2)
  (cond
   ((= a1 0) 0)
   ((= a2 0) 0)
   (t 1)))

(defun half-adder (a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; 3.28
(defun or-gate (a1 a2 output)
  (defun or-action-procedure ()
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 #'or-action-procedure)
  (add-action! a2 #'or-action-procedure)
  'ok)
(defun logical-or (a1 a2)
  (cond
   ((= a1 1) 1)
   ((= a2 1) 1)
   (t 0)))

;; 3.29
(defun or-gate2 (a b output)
  (let ((a1 (make-wire)) (b1 (make-wire)) (c (make-wire)))
    (inverter a a1)
    (inverter b b1)
    (and-gate a1 b1 c)
    (inverter c output)
    'ok))


;; 3.30
(defun ripple-carry (an bn sn c)
  (let ((c-out (make-wire)))
    (when (and (consp an)
               (consp bn)
               (consp sn))
      (full-adder (car an) (car bn) (car sn) c c-out)
      (ripple-carry (cdr an) (cdr bn) (cdr sn) c-out))
    'ok))


(defun make-wire ()
  (let ((signal-value 0)
        (action-procedures '()))
    (defun set-my-signal! (new-value)
      (if (not (= signal-value new-value))
          (progn
            (setq signal-value new-value)
            (call-each action-procedures))
        'done))
    (defun accept-action-procedure! (proc)
      (setq action-procedures
            (cons proc action-procedures))
      (funcall proc))
    (lambda (m)
      (cond ((eq m 'get-signal) signal-value)
            ((eq m 'set-signal!) #'set-my-signal!)
            ((eq m 'add-action!) #'accept-action-procedure!)
            ((eq m 'show-procedures) action-procedures)
            (t (error "Unknown operation: WIRE"))))))

(defun call-each (procedures)
  (if (null procedures)
      'done
    (progn
      (funcall (car procedures))
      (call-each (cdr procedures)))))
(defun get-signal (wire) (funcall wire 'get-signal))
(defun show-inner-procedures (wire) (funcall wire 'show-procedures))
(defun set-signal! (wire new-value)
  (funcall (funcall wire 'set-signal!) new-value))
(defun add-action! (wire action-procedure)
  (funcall (funcall wire 'add-action!) action-procedure))

(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-simulation-time the-agenda))
                  action
                  the-agenda))
(defun propagate ()
  (if (empty-agenda? the-agenda)
      'done
    (let ((first-item (first-agenda-item the-agenda)))
      (funcall first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(defun probe (name wire)
  (add-action! wire
               (lambda ()
                 (message "%s %d New-value = %d"
                          name
                          (current-simulation-time the-agenda)
                          (get-signal wire)))))


(defun make-time-segment (time queue)
  (cons time queue))
(defun segment-time (s) (car s))
(defun segment-queue (s) (cdr s))

(defun make-agenda ()
  (list 0))
(defun current-simulation-time (agenda) (car agenda))
(defun set-current-time! (agenda time)
  (setcar agenda time))
(defun segments (agenda) (cdr agenda))
(defun set-segments! (agenda segments)
  (setcdr agenda segments))
(defun first-segment (agenda) (car (segments agenda)))
(defun rest-segments (agenda) (cdr (segments agenda)))
(defun empty-agenda? (agenda) (null (segments agenda)))

;; 3.31 to install the action in the agenda

(defun add-to-agenda! (time action agenda)
  (defun belongs-before? (segments)
    (or (null segments)
        (< time (segment-time (car segments)))))
  (defun make-new-time-segment (time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (defun add-to-segments! (segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
            (setcdr segments
                    (cons (make-new-time-segment time action)
                          (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
      (add-to-segments! segments))))

(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(defun first-agenda-item (agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))



;; local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; sicp-chp3.el ends here
