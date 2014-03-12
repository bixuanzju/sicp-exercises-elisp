;;; chp3.el --- exercises from chp3 -*- lexical-binding: t -*-

;; Copyright (C) Jeremy Bi

;; Author: Jeremy Bi <bixuanzju@qq.com>
;; Maintainer: Jeremy Bi <bixuanzju@qq.com>
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
  (let* ((count 0)
         (mf (lambda (m)
               (cond
                ((eq m 'how-many-calls?)
                 count)
                ((eq m 'reset-count)
                 (setq count 0))
                (t (progn
                     (setq count (1+ count))
                     (funcall f m)))))))
    mf))
;; (fset 's (make-monitor 'sqrt))
;; (s 100)
;; (s 'how-many-calls?)
;; (s 'reset-count)

;; 3.3
(defun make-account (balance pwd)
  (let*
      ((withdraw (lambda (amount)
                   (if (>= balance amount)
                       (progn
                         (setq balance (- balance amount))
                         balance)
                     "Insufficient funds")))
       (deposit (lambda (amount)
                  (setq balance (+ balance amount))))
       (dispatch (lambda (secret m)
                   (if (eq secret pwd)
                       (cond ((eq m 'withdraw) withdraw)
                             ((eq m 'deposit) deposit)
                             (t (error "Unknown request")))
                     (lambda (_) "Incorrect password")))))
    dispatch))

;; (fset 'acc (make-account 100 'secret-password))
;; (funcall (acc 'secret-password 'withdraw) 40)
;; (funcall (acc 'secret-password 'deposit) 40)

;; 3.4
;; (setq acc (make-account2 100 'secret-password))
;; (funcall (funcall acc 'secret-password 'withdraw) 40)
;; (funcall (funcall acc 'secret-password 'deposit) 40)

;; 3.5
(defun estimate-integral (pred x1 x2 y1 y2 trials)
  (letrec
      ((iter (lambda (trials-remaining trials-passed)
               (cond ((= trials-remaining 0)
                      (/ trials-passed trials))
                     ((funcall pred x1 x2 y1 y2)
                      (funcall iter (- trials-remaining 1)
                               (1+ trials-passed)))
                     (t (funcall iter (- trials-remaining 1)
                                 trials-passed))))))
    (* (- x2 x1) (- y2 y1) (funcall iter trials 0.0))))

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
  (let* ((withdraw
          (lambda (amount)
            (if (>= balance amount)
                (progn (setq balance (- balance amount))
                       balance)
              "Insufficient funds")))
         (deposit (lambda (amount)
                    (setq balance (+ balance amount))))
         (add-pwd (lambda (npwd)
                    (push npwd pwds)))
         (dispatch
          (lambda (secret m)
            (if (memq secret pwds)
                (cond ((eq m 'withdraw)
                       withdraw)
                      ((eq m 'deposit)
                       deposit)
                      ((eq m 'add-pwd)
                       add-pwd)
                      (t (error "Unknown request")))
              (lambda (_)
                "Incorrect password")))))
    dispatch))

;; (setq peter-acc (make-account3 100 'open-sesame))
;; (funcall (funcall peter-acc 'open-sesame 'deposit) 30)

(defun make-joint (account pwd npwd)
  (progn
    (if (listp (funcall (funcall account pwd 'add-pwd) npwd))
        account
      (error "Incorrent password"))))

;; (setq paul-acc
;;       (make-joint peter-acc 'open-sesame 'rousebud))
;; (funcall (funcall peter-acc 'open-sesame 'withdraw) 30)

(defun last-pair (x)
  (if (null (cdr x)) x (last-pair (cdr x))))

(defun make-cycle (x)
  (setcdr (last-pair x) x)
  x)

;; 3.14
(defun mystery (x)
  (letrec
      ((myloop (lambda (x y)
                 (if (null x)
                     y
                   (let ((temp (cdr x)))
                     (setcdr x y)
                     (funcall myloop temp x))))))
    (funcall myloop x '())))


;; 3.16
(defun count-pair (x)
  (if (not (consp x))
      0
    (+ (count-pair (car x))
       (count-pair (cdr x))
       1)))

;; 3.17
(defun count-pair-c (x)
  (letrec ((temp '())
           (iter (lambda (xs)
                   (cond
                    ((not (consp xs)) 0)
                    ((memq xs temp) 0)
                    (t (setq temp (cons xs temp))
                       (+ 1
                          (funcall iter (car xs))
                          (funcall iter (cdr xs))))))))
    (funcall iter x)))

;; (setq a '(1))
;; (setq b (cons a a))
;; (setq c (cons b b))

;; 3.18
(defun detect-cycle (x)
  (letrec ((temp '())
           (iter (lambda (xs)
                   (cond
                    ((null xs) nil)
                    ((memq xs temp) t)
                    (t (setq temp (cons xs temp))
                       (funcall iter (cdr xs)))))))
    (funcall iter x)))

;; (setq a (make-cycle '(1 2 3 4)))

;; 3.19
(defun detect-cycle-clever (x)
  (letrec
      ((iter (lambda (a b)
               (cond
                ((eq a b) t)
                ((or (null a) (null b)) nil)
                (t (funcall iter (cdr a) (cdr (cdr b))))))))
    (funcall iter x (cdr-safe x))))


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

;; (setq q1 (make-queue))
;; (print-queue (insert-queue! q1 'a))
;; (print-queue (insert-queue! q1 'b))
;; (print-queue (delete-queue! q1))
;; (print-queue (delete-queue! q1))

;; 3.22
(defun make-queue2 ()
  (let* ((front-ptr '())
        (rear-ptr '())
        (empty-queue? (lambda ()
                        (null front-ptr)))
        (front-queue (lambda ()
                       (if (funcall empty-queue?)
                           (error "FRONT called with an empty queue")
                         (car front-ptr))))
        (insert-queue! (lambda (item)
                         (let ((new-pair (cons item '())))
                           (cond
                            ((funcall empty-queue?)
                             (setq front-ptr new-pair)
                             (setq rear-ptr new-pair))
                            (t (setcdr rear-ptr new-pair)
                               (setq rear-ptr new-pair)))
                           'ok)))
        (delete-queue! (lambda ()
                         (cond
                          ((funcall empty-queue?)
                           (error "DELETE called with an empty queue"))
                          (t (setq front-ptr (cdr front-ptr))))
                         'ok))
        (dispatch (lambda (m)
                    (cond
                     ((eq m 'empty-queue?) empty-queue?)
                     ((eq m 'front-queue) front-queue)
                     ((eq m 'insert-queue!) insert-queue!)
                     ((eq m 'delete-queue!) delete-queue!)
                     (t (error "Unknown command"))))))
    dispatch))

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
  (letrec
      ((iter (lambda (ptr lst)
               (if (null ptr) (reverse lst)
                 (funcall iter (cdr ptr) (cons (caar ptr) lst))))))
    (funcall iter (front-ptr-de dq) '())))

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
  (letrec ((local-table (list 'table))
           (assoc (lambda (key records)
                    (cond
                     ((null records) nil)
                     ((funcall same-key? (caar records) key) (car records))
                     (t (funcall assoc key (cdr records))))))
           (lookup (lambda (key1 key2)
                     (let ((subtable (assoc key1 (cdr local-table))))
                       (if subtable
                           (let ((record (assoc key2 (cdr subtable))))
                             (if record (cdr record) nil))
                         nil))))
           (insert! (lambda (key1 key2 value)
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
                      'ok))
           (dispatch (lambda (m)
                       (cond
                        ((eq m 'lookup-proc) lookup)
                        ((eq m 'insert-proc!) insert!)
                        (t (error "Unknown operation"))))))
    dispatch))

;; (setq t1 (make-table-local #'equal))
;; (funcall (funcall t1 'insert-proc!) 1 2 3)
;; (funcall (funcall t1 'lookup-proc) 1 2)

;; 3.25
(defun foldl (op acc lst)
  (cond
   ((null lst) acc)
   (t (foldl op (funcall op acc (car lst)) (cdr lst)))))

(defun make-table-g ()
  (letrec ((local-table (list '*table*))
           (lookup (lambda (key-lst)
                     (foldl
                      (lambda (records key)
                        (cond
                         ((null records) nil)
                         ((consp records)
                          (let ((record (assoc key records)))
                            (if record (cdr record) nil)))
                         (t nil)))
                      (cdr local-table)
                      key-lst)))
           (iter (lambda (subtable keys)
                   (cond
                    ((null keys)
                     (cons subtable nil))
                    ((consp (cdr subtable))
                     (cons subtable keys))
                    (t (let ((record (assoc (car keys) (cdr subtable))))
                         (if record
                             (funcall iter record (cdr keys))
                           (cons subtable keys)))))))
           (make-new (lambda (keys value)
                       (if (null (cdr keys))
                           (cons (car keys) value)
                         (list (car keys) (funcall make-new (cdr keys) value)))))
           (insert! (lambda (key-lst value)
                      (let ((last-table (car (iter local-table key-lst)))
                            (last-keys (cdr (iter local-table key-lst))))
                        (if (null last-keys)
                            (setcdr last-table value)
                          (setcdr last-table (cons (make-new last-keys value)
                                                   (cdr last-table))))))
                    'ok)
           (dispatch (lambda (m)
                       (cond
                        ((eq m 'lookup-proc) lookup)
                        ((eq m 'insert-proc!) insert!)
                        (t (error "Unknown operation"))))))
    dispatch))

;; (setq t2 (make-table-g))
;; (setq get-value (funcall t2 'lookup-proc))
;; (setq put-value (funcall t2 'insert-proc!))

;; (funcall put-value '(1 2 3) 4)
;; (funcall put-value '(1 2 5 6) 7)
;; (funcall put-value '(1 2 5) 0)
;; (funcall get-value '(1 2 5 6))

;; 3.26
(defun make-table-b ()
  (letrec ((local-table)
           (entry (lambda (tree) (car tree)))
           (branch-left (lambda (tree) (cadr tree)))
           (branch-right (lambda (tree) (caddr tree)))
           (make-tree (lambda (entry left right) (list entry left right)))
           (lookup (lambda (key)
                     (letrec
                         ((iter (lambda (table)
                                  (cond ((null table)
                                         nil)
                                        ((= (car (funcall entry table))
                                            key)
                                         (cdr (funcall entry table)))
                                        ((> (car (funcall entry table))
                                            key)
                                         (funcall iter (funcall branch-right table)))
                                        (t (funcall iter (funcall branch-left table)))))))
                       (funcall iter local-table))))
           (insert! (lambda (key value)
                      (letrec
                          ((iter (lambda (table)
                                   (cond
                                    ((null table)
                                     (funcall make-tree (cons key value) nil nil))
                                    ((= (car (funcall entry table)) key)
                                     (setcdr (funcall entry table) value)
                                     table)
                                    ((> (car (funcall entry table)) key)
                                     (funcall make-tree (funcall entry table)
                                                (funcall branch-left table)
                                                (funcall iter (funcall branch-right table))))
                                    (t (funcall make-tree
                                        (funcall entry table)
                                        (funcall iter (funcall branch-left table))
                                        (funcall branch-right table)))))))
                        (setq local-table (funcall iter local-table))
                        'ok)))
           (dispatch (lambda (m)
                       (cond
                        ((eq m 'lookup-proc) lookup)
                        ((eq m 'insert-proc!) insert!)
                        (t (error "Unknown operation"))))))
    dispatch))

;; TEST
;; (setq t1 (make-table-b))
;; (setq t2 (make-table-b))
;; (setq get-value (funcall t1 'lookup-proc))
;; (setq set-value (funcall t1 'insert-proc!))
;; (funcall set-value 2 4)
;; (funcall set-value 7 9)
;; (funcall set-value 1 5)
;; (funcall get-value 7)



(defun inverter (input output)
  (let ((invert-input
         (lambda ()
           (let ((new-value (logical-not (get-signal input))))
             (after-delay inverter-delay
                          (lambda () (set-signal! output new-value)))))))
    (add-action! input invert-input)
    'ok))
(defun logical-not (s)
  (cond
   ((= s 0) 1)
   ((= s 1) 0)
   (t (error "Invalid signal"))))

(defun and-gate (a1 a2 output)
  (let ((add-action-procedure
         (lambda ()
           (let ((new-value
                  (logical-and (get-signal a1) (get-signal a2))))
             (after-delay and-gate-delay
                          (lambda () (set-signal! output new-value)))))))
    (add-action! a1 add-action-procedure)
    (add-action! a2 add-action-procedure)
    'ok))
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
  (let ((or-action-procedure
         (lambda ()
           (let ((new-value
                  (logical-or (get-signal a1) (get-signal a2))))
             (after-delay or-gate-delay
                          (lambda () (set-signal! output new-value)))))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))
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
      (ripple-carry (cdr an) (cdr bn) (cdr sn) c-out))))


(defun make-wire ()
  (let* ((signal-value 0)
         (action-procedures '())
         (set-my-signal!
          (lambda (new-value)
            (if (not (= signal-value new-value))
                (progn
                  (setq signal-value new-value)
                  (call-each action-procedures))
              'done)))
         (accept-action-procedure!
          (lambda (proc)
            (setq action-procedures
                  (cons proc action-procedures))
            (funcall proc)))
         (dispatch
          (lambda (m)
            (cond ((eq m 'get-signal) signal-value)
                  ((eq m 'set-signal!) set-my-signal!)
                  ((eq m 'add-action!) accept-action-procedure!)
                  ((eq m 'show-procedures) action-procedures)
                  (t (error "Unknown operation: WIRE"))))))
    dispatch))

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
  (letrec
      ((belongs-before?
        (lambda (segments)
          (or (null segments)
              (< time (segment-time (car segments))))))
       (make-new-time-segment
        (lambda (time action)
          (let ((q (make-queue)))
            (insert-queue! q action)
            (make-time-segment time q))))
       (add-to-segments!
        (lambda (segments)
          (if (= (segment-time (car segments)) time)
              (insert-queue! (segment-queue (car segments))
                             action)
            (let ((rest (cdr segments)))
              (if (funcall belongs-before? rest)
                  (setcdr segments
                          (cons (funcall make-new-time-segment time action)
                                (cdr segments)))
                (funcall add-to-segments! rest))))))
       (segments (segments agenda)))
    (if (funcall belongs-before? segments)
        (set-segments!
         agenda
         (cons (funcall make-new-time-segment time action)
               segments))
      (funcall add-to-segments! segments))))

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

;; TEST
;; (setq the-agenda (make-agenda))
;; (setq inverter-delay 2)
;; (setq and-gate-delay 3)
;; (setq or-gate-delay 5)

;; (setq input-1 (make-wire))
;; (setq input-2 (make-wire))
;; (setq output (make-wire))
;; (setq carry (make-wire))

;; (probe 'output output)
;; (probe 'carry carry)

;; (half-adder input-1 input-2 output carry)
;; (set-signal! input-1 1)
;; (propagate)


(defun make-serializer ()
  (let ((mutex (make-mutex)))
    (lambda (p)
      (let ((serialized-p (lambda (&rest args)
                            (funcall mutex 'acquire)
                            (let ((val (apply p args)))
                              (funcall mutex 'release)
                              val))))
        serialized-p))))

(defun make-mutex ()
  (let* ((cell (list nil))
         (the-mutex (lambda (m)
                      (cond
                       ((eq m 'acquire)
                        (if (test-and-set! cell)
                            (the-mutex 'acquire)))
                       ((eq m 'release)
                        (clear! cell))))))
    the-mutex))
(defun clear! (cell)
  (setcar cell nil))
(defun test-and-set! (cell)
  (if (car cell)
      t
    (progn
      (setcar cell t)
      nil)))

(defun my-test ()
  (let* ((lst '())
         (add-item (lambda (i)
                     (setq lst (cons i lst))))
         (dispatch (lambda (m)
                     (cond
                      ((eq m 'add-item) add-item)
                      ((eq m 'show-list) lst)))))
    dispatch))

;; TEST
;; (setq t1 (make-serializer))
;; (setq test (funcall t1 (my-test)))
;; (funcall (funcall test 'add-item) 8)

;; 3.47
(defun make-semaphore (n)
  (let* ((mutex (make-mutex))
         (count n)
         (the-semaphore
          (lambda (m)
            (cond
             ((eq m 'acquire)
              (cond
               ((= count 1)
                (setq count 0)
                (funcall mutex 'accquire))
               ((= count 0)
                (funcall mutex 'accquire))
               (t (setq count (- count 1)))))
             ((eq m 'release)
              (if (= count 0)
                  (progn
                    (setq count 1)
                    (funcall mutex 'release))
                (setq count (1+ count))))))))
    the-semaphore))


(defconst the-empty-stream '())
(defun stream-null? (s)
  (null s))
(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(defun stream-map (proc s)
  (if (stream-null? s)
      the-empty-stream
    (cons-stream (funcall proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))
(defun stream-for-each (proc s)
  (if (stream-null? s)
      'done
    (progn
      (funcall proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (force (cdr stream)))
(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))
(defun memo-proc (proc)
  (let ((already-run?) (result))
    (lambda ()
      (if (not already-run?)
          (progn (setq result (funcall proc))
                 (setq already-run? t)
                 result)
        result))))
(defmacro delay (f)
  `(memo-proc (lambda () ,f)))
(defun force (d)
  (funcall d))

(defun stream-enumerate-interval (low high)
  (if (> low high)
      the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (1+ low)
                                            high))))
(defun stream-filter (pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((funcall pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (t (stream-filter pred (stream-cdr stream)))))

;;3.50
(defun stream-map-g (proc &rest argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
    (cons-stream
     (apply proc (mapcar #'stream-car argstreams))
     (apply #'stream-map-g (cons proc (mapcar #'stream-cdr argstreams))))))

(defun stream->list (stream n)
  (if (or (stream-null? stream) (= n 0)) '()
    (cons (stream-car stream) (stream->list (stream-cdr stream) (- n 1)))))

;; TEST
;; (setq s1 (stream-enumerate-interval 1 3))
;; (setq s2 (stream-enumerate-interval 4 6))
;; (stream->list (stream-map-g #'+ s1 s2) 2)

;; 3.51
(defun show (x)
  (message "%d" x)
  x)
;; (setq x (stream-map #'show
;;                     (stream-enumerate-interval 0 10)))
;; (stream-ref x 5)

;; 3.52
;; (setq sum 0)
;; (defun accum (x) (setq sum (+ x sum)))
;; (setq seq (stream-map-g #'accum
;;                       (stream-enumerate-interval 1 20)))
;; (setq y (stream-filter #'evenp seq))
;; (setq z
;;       (stream-filter (lambda (x) (= (% x 5) 0))
;;                      seq))
;; (stream-ref y 7)
;; (stream->list z)

;;
(defun integer-starting-from (n)
  (cons-stream n (integer-starting-from (1+ n))))

(defun divisible? (x y) (= (% x y) 0))

(defun sieve (stream)
    (cons-stream
     (stream-car stream)
     (sieve (stream-filter
             (lambda (x) (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(setq primes (sieve (integer-starting-from 2)))

(defun add-streams (s1 s2)
  (stream-map-g #'+ s1 s2))

(setq fibs (cons-stream
            0
            (cons-stream 1 (add-streams fibs (stream-cdr fibs)))))

(defun scale-stream (stream factor)
  (stream-map-g (lambda (x) (* factor x))
                stream))
(setq double (cons-stream 1 (scale-stream double 2)))

;; 3.53
(setq s (cons-stream 1 (add-streams s s)))

;; 3.54
(defun mul-streams (s1 s2)
  (stream-map-g #'* s1 s2))
(setq ones (cons-stream 1 ones))
(setq integers (cons-stream 1 (add-streams ones integers)))
(setq factorials
      (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

;; 3.55
(defun partial-sums (s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s))))

;; TEST
;; (stream->list (partial-sums integers) 8)

;; 3.56
(defun merge-streams (s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (t
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge-streams (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge-streams s1 (stream-cdr s2))))
                 (t (cons-stream
                     s1car
                     (merge-streams (stream-cdr s1) (stream-cdr s2)))))))))

(setq S
      (cons-stream
       1
       (let ((stream (merge-streams (scale-stream S 2)
                                    (scale-stream S 3))))
         (merge-streams stream (scale-stream S 5)))))

;; 3.58
(defun expand (num den radix)
  (cons-stream
   (/ (* num radix) den)
   (expand (% (* num radix) den) den radix)))

;; 3.59
(defun integrate-series (s)
  (letrec
      ((series (lambda (n stream)
                 (cons-stream
                  (* (stream-car stream)
                     (/ 1.0 n))
                  (funcall series (1+ n) (stream-cdr stream))))))
    (funcall series 1 s)))

(setq exp-series
      (cons-stream 1 (integrate-series exp-series)))
(setq cosine-series
      (cons-stream 1 (stream-map-g (lambda (n) (* -1 n))
                                   (integrate-series sine-series))))
(setq sine-series
      (cons-stream 0 (integrate-series cosine-series)))

;; 3.60
(defun mul-series (s1 s2)
  (cons-stream
   (* (stream-car s1)
      (stream-car s2))
   (add-streams
    (scale-stream (stream-cdr s1) (stream-car s2))
    (mul-series s1 (stream-cdr s2)))))

;; 3.61
(defun invert-unit-series (s)
  (cons-stream 1 (stream-map-g
                  (lambda (n) (* -1 n))
                  (mul-series (stream-cdr s) (invert-unit-series s)))))
;; TEST
(stream->list (invert-unit-series exp-series) 4)

;; 3.62
(defun div-series (s1 s2)
  (let ((scar (stream-car s2)))
    (if (= scar 0) (error "Divide by zero!")
      (stream-map-g
       (lambda (n) (/ n (float scar)))
       (mul-series s1 (invert-unit-series
                       (stream-map-g
                        (lambda (n) (/ n (float scar)))
                        s2)))))))

(setq tangent-series (div-series sine-series cosine-series))
;; TEST
;; (stream->list tangent-series 6)


(defun average (x y)
  (/ (+ x y) 2))

(defun sqrt-improve (guess x)
  (average guess (/ x guess)))

(defun negate (n)
  (* -1 n))

(defun sqrt-stream (x)
  (let ((guesses))
    (setq guesses (cons-stream
                   1.0
                   (stream-map-g (lambda (guess) (sqrt-improve guess x))
                                 guesses)))
    guesses))

(defun pi-summands (n)
  (cons-stream (/ 1.0 n)
               (stream-map-g #'negate (pi-summands (+ 2 n)))))
(setq pi-stream
      (scale-stream (partial-sums (pi-summands 1)) 4))

(defun euler-transformer (s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (expt (- s2 s1) 2)
                          (+ s0 (* -2 s1) s2)))
                 (euler-transformer (stream-cdr s)))))

(defun make-tableau (transform s)
  (cons-stream s (make-tableau transform (funcall transform s))))

(defun accelerate-sequence (transform s)
  (stream-map-g #'stream-car (make-tableau transform s)))

;; 3.64
(defun stream-limit (s tol)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s0 s1)) tol)
        the-empty-stream
      (cons-stream
       s0
       (stream-limit (stream-cdr s) tol)))))

(defun sqrt (x tol)
  (stream-limit (sqrt-stream x) tol))

;; 3.65
(defun log-summands (n)
  (cons-stream
   (/ 1.0 n)
   (stream-map-g #'negate (log-summands (1+ n)))))

(setq log2 (accelerate-sequence #'euler-transformer
                                (partial-sums (log-summands 1))))

(defun interleave (s1 s2)
  (if (stream-null? s1)
      s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(defun stream-pairs (s1 s2)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (interleave
    (stream-map-g (lambda (x) (list (stream-car s1) x))
                  (stream-cdr s2))
    (stream-pairs (stream-cdr s1) (stream-cdr s2)))))

(setq integers (integer-starting-from 1))

;; 3.67
(defun stream-all-pairs (s1 s2)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (interleave
    (stream-map-g (lambda (x) (list (stream-car s1) x))
                  (stream-cdr s2))
    (interleave
     (stream-map-g (lambda (x) (list x (stream-car s2)))
                   (stream-cdr s1))
     (stream-all-pairs (stream-cdr s1) (stream-cdr s2))))))

;; TEST
;; (stream->list (stream-all-pairs integers integers) 5)

;; 3.68
(defun wrong-pairs (s1 s2)
  (interleave
   (stream-map-g (lambda (x) (list (stream-car s1) x))
                 s2)
   (wrong-pairs (stream-cdr s1) (stream-cdr s2))))

;; (stream->list (wrong-pairs integers integers) 2)

;; 3.69
(defun stream-triples (s1 s2 s3)
  (cons-stream
   (list (stream-car s1) (stream-car s2) (stream-car s3))
   (interleave
    (stream-map-g (lambda (x) (cons (stream-car s1) x))
                  (stream-pairs s2 (stream-cdr s3)))
    (stream-triples (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))))

(defun square (x)
  (* x x))

;; Don't try to run it, Emacs can't afford
;; (setq pythagorean-triples (stream-filter
;;                            (lambda (x)
;;                              (let ((x1 (car x))
;;                                    (x2 (cadr x))
;;                                    (x3 (caddr x)))
;;                                (= (+ (square x1) (square x2)) (square x3))))
;;                            (stream-triples integers integers integers)))

;; 3.70
(defun merge-weighted (s1 s2 weight)
  (let ((s1car (stream-car s1))
        (s2car (stream-car s2)))
    (cond ((< (funcall weight (car s1car) (cadr s1car))
              (funcall weight (car s2car) (cadr s2car)))
           (cons-stream
            s1car
            (merge-weighted (stream-cdr s1) s2 weight)))
          ((> (funcall weight (car s1car) (cadr s1car))
              (funcall weight (car s2car) (cadr s2car)))
           (cons-stream
            s2car
            (merge-weighted s1 (stream-cdr s2) weight)))
          (t (cons-stream
              s1car
              (cons-stream
               s2car
               (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)))))))

(defun stream-pairs-g (s1 s2 weight)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (stream-map-g (lambda (x) (list (stream-car s1) x))
                  (stream-cdr s2))
    (stream-pairs-g (stream-cdr s1) (stream-cdr s2) weight)
    weight)))

(setq S1 (stream-pairs-g integers integers (lambda (x y) (+ x y))))

(defun not-divisible (n)
  (not (or (divisible? n 2)
           (divisible? n 3)
           (divisible? n 5))))
(setq S2 (stream-filter
          (lambda (pair)
            (let ((x (car pair))
                  (y (cadr pair)))
              (and (not-divisible x) (not-divisible y))))
          (stream-pairs-g integers integers
                          (lambda (x y) (+ (* 2 x) (* 3 y) (* x y))))))

;; 3.71
(defun cube (x)
  (* x x x))
(defun cube-sum (x y)
  (+ (cube x) (cube y)))
(defun search-pair (s)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (if (= (cube-sum (car s1) (cadr s1))
           (cube-sum (car s2) (cadr s2)))
        (cons-stream s1
                     (search-pair (stream-cdr s)))
      (search-pair (stream-cdr s)))))
(setq ramanujan-number
      (stream-map-g
       (lambda (pair)
         (+ (cube (car pair))
            (cube (cadr pair))))
       (search-pair (stream-pairs-g integers
                                    integers
                                    #'cube-sum))))
;; (stream->list ramanujan-number 6)

;; 3.73
(defun integral (integrand initial-value dt)
  (let ((int))
    (setq int (cons-stream initial-value
                         (add-streams (scale-stream integrand dt)
                                      int)))
    int))

(defun RC (R C dt)
  (lambda (i vo)
    (add-streams
     (scale-stream i R)
     (integral (scale-stream i (/ 1.0 C))
               vo
               dt))))
;; (setq RC1 (RC 5 1 0.5))

;; 3.74
(defun sign-change-detector (x y)
  (cond
   ((or (and (> x 0) (< y 0))
        (and (= x 0) (< y 0)))
    1)
   ((or (and (< x 0) (> y 0))
        (and (< x 0) (= y 0)))
    -1)
   (t 0)))

(defun make-zero-crossing (input-stream last-value)
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossing
    (stream-cdr input-stream)
    (stream-car input-stream))))
(defun list->stream (lst)
  (if (null lst) the-empty-stream
    (cons-stream
     (car lst)
     (list->stream (cdr lst)))))

(setq sense-data (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(setq zero-crossing (stream-map-g #'sign-change-detector
                                  sense-data
                                  (cons-stream 0 sense-data)))

;; 3.75
(defun make-zero-crossing2 (input-stream last-value previous-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    previous-value)
                 2.0)))
    (cons-stream
     (sign-change-detector avpt last-value)
     (make-zero-crossing2
      (stream-cdr input-stream)
      avpt
      (stream-car input-stream)))))
(setq zero-crossing2 (make-zero-crossing2
                             sense-data
                             0
                             0))

;; 3.76
(defun smooth (stream)
  (stream-map-g (lambda (x y) (/ (+ x y) 2.0))
                stream
                (cons-stream 0 stream)))
(setq zero-crossing3 (let ((smothed (smooth sense-data)))
                       (stream-map-g #'sign-change-detector
                                     smothed
                                     (cons-stream 0 smothed))))


(defun integral2 (delayed-integrand initial-value dt)
  (let ((int))
    (setq int
          (cons-stream
           initial-value
           (let ((integrand (force delayed-integrand)))
             (add-streams (scale-stream integrand dt)
                          int))))
    int))
(defun solve (f y0 dt)
  (let ((y)
        (dy))
    (setq y (integral2 (delay dy) y0 dt))
    (setq dy (stream-map-g f y))
    y))

;; 3.77
(defun integral3 (delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
       (integral3 (delay (stream-cdr integrand))
                  (+ (* dt (stream-car integrand))
                     initial-value)
                  dt)))))

;; 3.78
(defun solve-2nd (a b y0 dy0 dt)
  (let ((y) (dy) (ddy))
    (setq y (integral2 (delay dy) y0 dt))
    (setq dy (integral2 (delay ddy) dy0 dt))
    (setq ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
    y))

;; 3.79
(defun solve-2nd-g (f y0 dy0 dt)
  (let ((y) (dy) (ddy))
    (setq y (integral2 (delay dy) y0 dt))
    (setq dy (integral2 (delay ddy) dy0 dt))
    (setq ddy (stream-map-g f dy y))
    y))

;; 3.80
(defun RCL (R L C dt)
  (lambda (vc0 il0)
    (let ((vc) (il) (dvc) (dil))
      (setq vc (integral2 (delay dvc) vc0 dt))
      (setq il (integral2 (delay dil) il0 dt))
      (setq dvc (scale-stream il (/ -1.0 C)))
      (setq dil (add-streams (scale-stream il (* -1 (/ R (float L))))
                             (scale-stream vc (/ 1.0 L))))
      (stream-map-g (lambda (x y) (cons x y)) vc il))))

;; TEST
;; (setq RCL0 (RCL 1 1 0.2 0.1))
;; (stream->list (funcall RCL0 10 0) 12)

;;; AWESOME chapter!


;; local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; chp3.el ends here
