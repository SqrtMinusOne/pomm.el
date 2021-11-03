;;; pomm.el --- Yet another Pomodoro timer implementation -*- lexical-binding: t -*-

;; Copyright (C) 2021 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (alert "1.2") (seq "2.22"))
;; Homepage: https://github.com/SqrtMinusOne/pomm.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:
(require 'alert)
(require 'seq)

(defgroup pomm nil
  "Yet another Pomodoro timer implementation."
  :group 'tools)

(defcustom pomm-work-period 25
  "Number of minutes for a work period."
  :group 'pomm
  :type 'integer)

(defcustom pomm-short-break-period 5
  "Number of minutes for a break period."
  :group 'pomm
  :type 'integer)

(defcustom pomm-long-break-period 25
  "Number of minutes for a long break period."
  :group 'pomm
  :type 'integer)

(defcustom pomm-number-of-periods 4
  "Number of periods before a long break."
  :group 'pomm
  :type 'integer)

(defcustom pomm-short-break-message "Take a short break!"
  "Message for a start of a short break period."
  :group 'pomm
  :type 'string)

(defcustom pomm-long-break-message "Take a longer break!"
  "Message for a start of a long break period."
  :group 'pomm
  :type 'string)

(defcustom pomm-ask-before-long-break t
  "Ask a user whether to do a long break or stop the pomodoros."
  :group 'pomm
  :type 'boolean)

(defcustom pomm-work-message "Time for work!"
  "Message for a start of a work period."
  :group 'pomm
  :type 'string)

(defcustom pomm-state-file-location
  (concat user-emacs-directory "pomm")
  "Location of the pomm state file."
  :group 'pomm
  :type 'string)

(defvar pomm--state nil
  "Current state of pomm.el.

This is an alist of with the following keys:
- status: either 'stopped, 'paused or 'running
- current: an alist with a current period
- history: a list with today's history
- last-changed-time: a timestamp of a last change in status

Current period is also an alist with the following keys:
- kind: either 'short-break, 'long-break or 'work
- start-time: start timestamp
- effective-start-time: start timestamp, corrected for pauses
- iteration: number the current pomodoro iteration")

(defvar pomm--timer nil
  "A variable for the pomm timer.")

(defun pomm-reset ()
  "Reset the pomodoro timer."
  (interactive)
  (setq pomm--state
        `((status . stopped)
          (current . nil)
          (history . nil)
          (last-changed-time ,(time-convert nil 'integer)))))

(defun pomm--init-state ()
  "Initialize the pomodoro timer state."
  (pomm-reset))

(defun pomm--dispatch-notification (kind)
  "Dispatch a notification about a start of a period.

KIND is the same as in `pomm--state'"
  (alert
   (pcase kind
     ('short-break pomm-short-break-message)
     ('long-break pomm-long-break-message)
     ('work pomm-work-message))
   :title "Pomodoro"))

(defun pomm--new-iteration ()
  "Initialize state as a new iteration of pomodoro."
  (setf (alist-get 'current pomm--state)
        `((kind . work)
          (start-time . ,(time-convert nil 'integer))
          (effective-start-time . ,(time-convert nil 'integer))
          ;; Maximum iteration in history + 1 or 0
          (iteration . ,(1+ (seq-max
                             (cons 0
                                   (mapcar
                                    (lambda (h) (alist-get 'iteration h))
                                    (alist-get 'history pomm--state)))))))
        (alist-get 'last-changed-time pomm--state) (time-convert nil 'integer)
        (alist-get 'status pomm--state) 'running)
  (pomm--dispatch-notification 'work))

(defun pomm--get-kind-length (kind)
  "Get a length of period KIND in seconds."
  (* 60
     (pcase kind
       ('short-break pomm-short-break-period)
       ('long-break pomm-long-break-period)
       ('work pomm-work-period)
       (_ 0))))

(defun pomm--need-switch-p ()
  "Check if it is necessary to switch a period."
  (< (+ (alist-get 'effective-start-time (alist-get 'current pomm--state))
        (pomm--get-kind-length
         (alist-get 'kind (alist-get 'current pomm--state))))
     (time-convert nil 'integer)))

(defun pomm--store-current-to-history ()
  "Store the current pomodoro state to the history."
  (let* ((current-kind (alist-get 'kind (alist-get 'current pomm--state)))
         (current-iteration (alist-get 'iteration (alist-get 'current pomm--state)))
         (start-time (alist-get 'start-time (alist-get 'current pomm--state)))
         (end-time (time-convert nil 'integer))
         (paused-time (- end-time
                         start-time
                         (pomm--get-kind-length current-kind))))
    (push `((kind . ,current-kind)
            (iteration . ,current-iteration)
            (start-time . ,start-time)
            (end-time . ,end-time)
            (paused-time . ,paused-time))
          (alist-get 'history pomm--state))))

(defun pomm--switch-to-next ()
  "Switch to the next period."
  (let* ((current-kind (alist-get 'kind (alist-get 'current pomm--state)))
         (current-iteration (alist-get 'iteration (alist-get 'current pomm--state)))
         (work-periods (+ (seq-count
                           (lambda (item)
                             (and (= (alist-get 'iteration item) current-iteration)
                                  (eq 'work (alist-get 'kind item))))
                           (alist-get 'history pomm--state))
                          (if (eq current-kind 'work) 1 0)))
         (next-kind (cond
                     ((and (eq current-kind 'work)
                           (>= work-periods pomm-number-of-periods))
                      'long-break)
                     ((and (eq current-kind 'work)
                           (< work-periods pomm-number-of-periods))
                      'short-break)
                     (_ 'work)))
         (next-iteration (if (eq current-kind 'long-break)
                             (+ current-iteration 1)
                           current-iteration)))
    (pomm--store-current-to-history)
    (if (or (not (eq next-kind 'long-break))
            (not pomm-ask-before-long-break)
            (y-or-n-p "Start a long break (y) or end the pomodors (n)? "))
        (progn
          (setf (alist-get 'current pomm--state)
                `((kind . ,next-kind)
                  (start-time . ,(time-convert nil 'integer))
                  (effective-start-time . ,(time-convert nil 'integer))
                  (iteration . ,next-iteration)))
          (pomm--dispatch-notification next-kind))
      (setf (alist-get 'current pomm--state) nil)
      (setf (alist-get 'status pomm--state) 'stopped))))

(defun pomm--on-tick ()
  "A function to be ran on a timer tick."
  (pcase (alist-get 'status pomm--state)
    ('stopped (when pomm--timer (cancel-timer pomm--timer)))
    ('paused nil)
    ('running
     (when (pomm--need-switch-p)
       (pomm--switch-to-next)))))

;;;###autoload
(defun pomm-start ()
  "Start or continue the pomodoro timer."
  (interactive)
  (unless pomm--state
    (pomm--init-state))
  (cond
   ((eq (alist-get 'status pomm--state) 'stopped) (pomm--new-iteration))
   ((eq (alist-get 'status pomm--state) 'paused)
    (setf (alist-get 'status pomm--state) 'running
          (alist-get 'effective-start-time (alist-get 'current pomm--state))
          (+ (alist-get 'effective-start-time (alist-get 'current pomm--state))
             (- (time-convert nil 'integer) (alist-get 'last-changed-time pomm--state)))
          (alist-get 'last-changed-time pomm--state) (time-convert nil 'integer))))
  (unless pomm--timer
    (setq pomm--timer (run-with-timer 0 1 'pomm--on-tick))))

(defun pomm-stop ()
  "Stop the current iteration of the pomodoro timer."
  (interactive)
  (pomm--store-current-to-history)
  (setf (alist-get 'status pomm--state) 'stopped
        (alist-get 'current pomm--state) nil
        (alist-get 'last-changed-time pomm--state) (time-convert nil 'integer)))

(defun pomm-pause ()
  "Pause the pomodoro timer."
  (interactive)
  (setf (alist-get 'status pomm--state) 'paused
        (alist-get 'last-changed-time pomm--state) (time-convert nil 'integer)))

(provide 'pomm)
;;; pomm.el ends here
