;;; pomm-third-time.el --- Implementation of the third time technique in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
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

;; This is an implementation of the third time technique.  Take a look
;; at the `pomm-third-time' function for the general idea.
;;
;; This file reuses some parts from the main `pomm' file, but a lot of
;; functionality is duplicated.

;;; Code:
(require 'pomm)
(require 'calc)

(defgroup pomm-third-time nil
  "Third time timer implementation."
  :group 'pomm)

(defcustom pomm-third-time-fraction "1/3"
  "Fraction of break time to work time.

Can be string or number, a string is interpreted with
`calc-eval'."
  :group 'pomm-third-time
  :type '(choice (string :tag "String")
                 (number :tag "Number")))

(defcustom pomm-third-time-on-status-changed-hook nil
  "A hook to run on a status change."
  :group 'pomm-third-time
  :type 'hook)

(defcustom pomm-third-time-on-tick-hook nil
  "A hook to run on every tick when the timer is running."
  :group 'pomm-third-time
  :type 'hook)

(defcustom pomm-third-time-state-file-location
  (locate-user-emacs-file "pomm-third-time")
  "Location of the pomm-third-time state file."
  :group 'pomm-third-time
  :type 'string)

(defcustom pomm-third-time-break-message "Take a break!"
  "Message for a start of a short break period."
  :group 'pomm
  :type 'string)

(defvar pomm-third-time--state nil
  "The current state of pomm-third-time.el.

This is an alist with the following keys:
- status: either 'stopped or 'running
  (having a pause state seems to make little sense here)
- current: an alist with a current period
- history: a list with history for today
- last-changed-time: a timestamp of the last change in status
- context: a string that describes the current task

'current is an alist with the following keys:
- kind: either 'work or 'break
- start-time: start timestamp
- break-time-bank: break time, postpone from previous periods
- iteration: number of the current iteration

'history is a list of alists with the following keys:
- kind: same as in 'current
- iteration
- start-time: start timestamp
- end-time: end timestamp")

(defvar pomm-third-time--timer nil
  "A variable for the Third Time timer.")

(defun pomm-third-time--do-reset ()
  "Reset the Third Time timer state."
  (when pomm-third-time--timer
    (cancel-timer pomm-third-time--timer)
    (setq pomm-third-time--timer nil))
  (setq pomm-third-time--state
        `((status . , 'stopped)
          (current . ,nil)
          (history . ,nil)
          (last-changed-time ,(time-convert nil 'integer)))
        pomm-current-mode-line-string "")
  (setf (alist-get 'status pomm-third-time--state) 'stopped)
  (run-hooks 'pomm-on-status-changed-hook))

(defun pomm-third-time--init-state ()
  "Initialize the Third Time timer state."
  (add-hook 'pomm-third-time-on-status-changed-hook #'pomm-third-time--save-state)
  ;; TODO (add-hook 'pomm-on-status-changed-hook #'pomm-third-time--maybe-save-csv)
  (add-hook 'pomm-third-time-on-status-changed-hook
            #'pomm-third-time--dispatch-current-sound)
  (add-hook 'pomm-mode-line-mode-hook
            #'pomm-third-time--setup-mode-line)
  (pomm-third-time--setup-mode-line)
  (if (or (not (file-exists-p pomm-third-time-state-file-location))
          (not pomm-third-time-state-file-location))
      (pomm-third-time--do-reset)
    (with-temp-buffer
      (insert-file-contents pomm-third-time-state-file-location)
      (let ((data (buffer-substring (point-min) (point-max))))
        (if (not (string-empty-p data))
            (setq pomm-third-time--state (car (read-from-string data)))
          (pomm-third-time--do-reset)))))
  (pomm-third-time--cleanup-old-history))

(defun pomm-third-time--save-state ()
  "Save the current Pomodoro timer state."
  (when pomm-third-time-state-file-location
    (with-temp-file pomm-third-time-state-file-location
      (insert (prin1-to-string pomm-third-time--state)))))

(defun pomm-third-time--cleanup-old-history ()
  "Clear history of previous days from the Pomodoro timer."
  (let ((cleanup-time (decode-time)))
    (setf (decoded-time-second cleanup-time) 0
          (decoded-time-minute cleanup-time) 0
          (decoded-time-hour cleanup-time) pomm-history-reset-hour)

    (let ((cleanup-timestamp (time-convert (encode-time cleanup-time) 'integer)))
      (setf (alist-get 'history pomm-third-time--state)
            (seq-filter
             (lambda (item)
               (> (alist-get 'start-time item) cleanup-timestamp))
             (alist-get 'history pomm-third-time--state))))))

(defun pomm-third-time-reset ()
  "Reset the Third Time timer."
  (interactive)
  (when (y-or-n-p "Are you sure you want to reset the Third Time timer? ")
    (pomm-third-time--do-reset)))

(defun pomm-third-time--dispatch-current-sound ()
  "Dispatch an appropriate sound for the current state of the timer."
  (cond
   ((eq (alist-get 'status pomm-third-time--state) 'stopped)
    (pomm--maybe-play-sound 'stop))
   ((eq (alist-get 'status pomm-third-time--state) 'running)
    (pomm--maybe-play-sound
     (alist-get 'kind (alist-get 'current pomm-third-time--state))))))

(defun pomm-third-time--fraction ()
  "Get fraction of break time to work time."
  (if (stringp pomm-third-time-fraction)
      (string-to-number (calc-eval pomm-third-time-fraction))
    pomm-third-time-fraction))

(defun pomm-third-time--current-period-time ()
  "Get the time spent in the current period."
  (if-let ((time (alist-get 'start-time (alist-get 'current pomm-third-time--state))))
      (- (time-convert nil 'integer) time)
    0))

(defun pomm-third-time--break-time ()
  "Get the available break time."
  (max
   (+ (float
       (or (alist-get 'break-time-bank
                      (alist-get 'current pomm-third-time--state))
           0))
      (pcase (alist-get 'kind (alist-get 'current pomm-third-time--state))
        ('work (* (pomm-third-time--fraction)
                  (pomm-third-time--current-period-time)))
        ('break (- (pomm-third-time--current-period-time)))
        ('nil 0)))
   0))

(defun pomm-third-time--need-switch-p ()
  "Check if the break period has to end."
  (and
   (eq (alist-get 'kind (alist-get 'current pomm-third-time--state)) 'break)
   (<= (pomm-third-time--break-time) 0)))

(defun pomm-third-time--store-current-to-history ()
  "Store the timer state to history."
  (let ((current-kind (alist-get 'kind (alist-get 'current pomm-third-time--state)))
        (current-start-time (alist-get 'start-time
                                       (alist-get 'current pomm-third-time--state)))
        (current-iteration (alist-get 'iteration
                                      (alist-get 'current pomm-third-time--state))) )
    (when current-kind
      (push `((kind . ,current-kind)
              (start-time . ,current-start-time)
              (end-time . ,(time-convert nil 'integer)))
            (alist-get 'history pomm-third-time--state)))))

(defun pomm-third-time--format-period (seconds)
  "Format SECONDS into string."
  (if (>= seconds (* 60 60))
      (format-seconds "%.2h:%.2m:%.2s" seconds)
    (format-seconds "%.2m:%.2s" seconds)))

(defun pomm-third-time--dispatch-notification (kind)
  "Dispatch a notification about a start of a period.

KIND is the same as in `pomm--state'"
  (alert
   (pcase kind
     ('break (concat pomm-third-time-break-message)
             (format "\nTime available: %s"
                     (pomm-third-time--format-period
                      (pomm-third-time--break-time))))
     ('work (concat pomm-work-message
                    (when (> (pomm-third-time--break-time) 0)
                      (format "\nBreak time remaining: %s"
                              (pomm-third-time--format-period
                               (pomm-third-time--break-time)))))))
   :title "Pomodoro"))

(defun pomm-third-time--switch ()
  "Switch between periods."
  (let* ((current-kind (alist-get 'kind (alist-get 'current pomm-third-time--state)))
         (break-time (pomm-third-time--break-time))
         (iteration (alist-get 'iteration
                               (alist-get 'current pomm-third-time--state)))
         (next-kind (pcase current-kind
                      ('work 'break)
                      ('break 'work))))
    (setf (alist-get 'current pomm-third-time--state)
          `((kind . ,next-kind)
            (start-time . ,(time-convert nil 'integer))
            (break-time-bank . ,break-time)
            (iteration . ,iteration)))
    (pomm-third-time--store-current-to-history)
    (pomm-third-time--dispatch-notification next-kind)
    (pomm-third-time--save-state)
    (run-hooks 'pomm-third-time-on-status-changed-hook)))

(defun pomm-third-time--on-tick ()
  "A function to execute on each time tick."
  (pcase (alist-get 'status pomm-third-time--state)
    ('stopped (when pomm-third-time--timer
                (cancel-timer pomm-third-time--timer)
                (setq pomm-third-time--timer nil)))
    ('running
     (when (pomm-third-time--need-switch-p)
       (pomm-third-time--switch))
     (run-hooks 'pomm-third-time-on-tick-hook)
     (when (eq (alist-get 'kind (alist-get 'current pomm-third-time--state)) 'work)
       (pomm--maybe-play-sound 'tick)))))

(defun pomm-third-time--new-iteration ()
  "Start a new iteration of the Third Time timer."
  (setf (alist-get 'current pomm-third-time--state)
        `((kind . work)
          (start-time . ,(time-convert nil 'integer))
          (break-time-bank . 0)
          (iteration . ,(1+ (seq-max
                             (cons 0
                                   (mapcar
                                    (lambda (h) (alist-get 'iteration h))
                                    (alist-get 'history pomm-third-time--state)))))))
        (alist-get 'status pomm-third-time--state) 'running
        (alist-get 'last-changed-time pomm-third-time--state) (time-convert nil 'integer))
  (pomm-third-time--dispatch-notification 'work))

;;;###autoload
(defun pomm-third-time-start ()
  "Start the Third Time timer.

Take a look at the `pomm-third-time' function for more details."
  (interactive)
  (unless pomm-third-time--state
    (pomm-third-time--init-state))
  (pcase (alist-get 'status pomm-third-time--state)
    ('stopped (pomm-third-time--new-iteration)
              (run-hooks 'pomm-third-time-on-status-changed-hook))
    ('running (message "The timer is running!")))
  (unless pomm-third-time--timer
    (setq pomm--third-time--timer (run-with-timer 0 1 'pomm-third-time--on-tick))))

(defun pomm-third-time-stop ()
  "Stop the Third Time timer."
  (interactive)
  (pcase (alist-get 'status pomm-third-time--state)
    ('stopped (message "The timer is already stopped!"))
    ('running
     (when (y-or-n-p "This will reset the accumulated break time.  Continue? ")
       (pomm-third-time--store-current-to-history)
       (setf (alist-get 'status pomm-third-time--state) 'stopped
             (alist-get 'current pomm-third-time--state) nil
             (alist-get 'last-changed-time pomm-third-time--state)
             (time-convert nil 'integer))
       (run-hooks 'pomm-third-time-on-status-changed-hook)
       (when pomm-reset-context-on-iteration-end
         (setf (alist-get 'context pomm-third-time--state) nil))))))

(defun pomm-third-time-switch ()
  "Switch between work and break in the Third Time timer."
  (interactive)
  (pomm-third-time--switch))

(defun pomm-third-time-format-mode-line ()
  "Format mode string for the Third Time timer."
  (let ((current-status (alist-get 'status pomm-third-time--state)))
    (if (or (eq current-status 'stopped)
            (not (alist-get 'current pomm-third-time--state)))
        ""
      (let ((current-kind (alist-get 'kind (alist-get 'current pomm-third-time--state)))
            (break-time (pomm-third-time--break-time)))
        (format "[%s] %s (%s) "
                current-kind
                (pomm-third-time--format-period
                 (pomm-third-time--current-period-time))
                (pomm-third-time--format-period
                 (pomm-third-time--break-time)))))))

(defun pomm-third-time-update-mode-string ()
  "Update modeline for the Third Time timer."
  (setq pomm-current-mode-line-string (pomm-third-time-format-mode-line)))

(defun pomm-third-time--setup-mode-line ()
  "Setup `pomm-mode-line-mode' to work with `pomm-third-time'."
  (if pomm-mode-line-mode
      (progn
        (add-hook 'pomm-third-time-on-tick-hook #'pomm-third-time-update-mode-string)
        (add-hook 'pomm-third-time-on-tick-hook #'force-mode-line-update)
        (add-hook 'pomm-third-time-on-status-changed-hook #'pomm-third-time-update-mode-string)
        (add-hook 'pomm-third-time-on-status-changed-hook #'force-mode-line-update))
    (remove-hook 'pomm-third-time-on-tick-hook #'pomm-third-time-update-mode-string)
    (remove-hook 'pomm-third-time-on-tick-hook #'force-mode-line-update)
    (remove-hook 'pomm-third-time-on-status-changed-hook #'pomm-third-time-update-mode-string)
    (remove-hook 'pomm-third-time-on-status-changed-hook #'force-mode-line-update)))

(provide 'pomm-third-time)
;;; pomm-third-time.el ends here
