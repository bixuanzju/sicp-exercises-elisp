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
  (defun dispatch (secret m)
    (if (eq secret pwd)
        (cond ((eq m 'withdraw) #'withdraw)
              ((eq m 'deposit) #'deposit)
              (t (error "Unknown request")))
      (lambda (_) "Incorrect password")))
  #'dispatch)

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
    (defun dispatch (secret m)
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
              "Incorrect password")))))
    #'dispatch))

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
  (defun dispatch (secret m)
    (if (memq secret pwds)
        (cond ((eq m 'withdraw) #'withdraw)
              ((eq m 'deposit) #'deposit)
              ((eq m 'add-pwd) #'add-pwd)
              (t (error "Unknown request")))
      (lambda (_) "Incorrect password")))
  #'dispatch)

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



;; local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; sicp-chp3.el ends here
