;;; pomm-third-time.el --- Implementation of the third time technique in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

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
(require 'alert)
(require 'transient)
(require 'calc)

;; XXX optional dependency on org-clock
(declare-function org-clock-in-last "org-clock")
(declare-function org-clock-out "org-clock")
(defvar org-clock-current-task)

(defgroup pomm-third-time nil
  "Third Time timer implementation."
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
  "Location of the `pomm-third-time' state file."
  :group 'pomm-third-time
  :type 'string)

(defcustom pomm-third-time-break-message "Take a break!"
  "Message for a start of a short break period."
  :group 'pomm
  :type 'string)

(defcustom pomm-third-time-csv-history-file nil
  "If non-nil, save timer history in a CSV format.

The parent directory has to exist!

A new entry is written whenever the timer changes status or kind
of period.  The format is as follows:
- timestamp
- status
- kind
- iteration
- break-time-remaining
- context"
  :group 'pomm-third-time
  :type '(choice (string :tag "Path")
                 (const nil :tag "Do not save")))

(defvar pomm-third-time--state nil
  "The current state of the Third Time timer.

This is an alist with the following keys:
- status: either \\='stopped or \\='running
  (having a pause state seems to make little sense here)
- current: an alist with a current period
- history: a list with history for today
- last-changed-time: a timestamp of the last change in status
- context: a string that describes the current task

\\='current is an alist with the following keys:
- kind: either \\='work or \\='break
- start-time: start timestamp
- break-time-bank: break time, postpone from previous periods
- iteration: number of the current iteration

\\='history is a list of alists with the following keys:
- kind: same as in \\='current
- iteration
- start-time: start timestamp
- end-time: end timestamp
- context: context")

(defvar pomm-third-time--timer nil
  "A variable for the Third Time timer.")

(defun pomm-third-time--do-reset ()
  "Reset the Third Time timer state."
  (interactive)
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
  (run-hooks 'pomm-third-time-on-status-changed-hook))

(defun pomm-third-time--init-state ()
  "Initialize the Third Time timer state."
  (add-hook 'pomm-third-time-on-status-changed-hook #'pomm-third-time--save-state)
  (add-hook 'pomm-third-time-on-status-changed-hook #'pomm-third-time--maybe-save-csv)
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
  (pomm-third-time--cleanup-old-history)
  (when (eq (alist-get 'status pomm-third-time--state) 'running)
    (setq pomm--timer (run-with-timer 0 1 #'pomm-third-time--on-tick))))

(defun pomm-third-time--save-state ()
  "Save the current Third Time timer state."
  (when pomm-third-time-state-file-location
    (with-temp-file pomm-third-time-state-file-location
      (insert (prin1-to-string pomm-third-time--state)))))

(defun pomm-third-time--cleanup-old-history ()
  "Clear history of previous days from the Third Time timer."
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

(defun pomm-third-time--maybe-save-csv ()
  "Log the current state of the timer to a CSV history file.

Set `pomm-third-time-csv-history-file' to customize the file location.
If the variable is nil, the function does nothing."
  (when pomm-third-time-csv-history-file
    (unless (file-exists-p pomm-third-time-csv-history-file)
      (with-temp-file pomm-third-time-csv-history-file
        (insert "timestamp,status,period,iteration,break-time-remaining,context\n")))
    (write-region
     (format "%s,%s,%s,%d,%d,%s\n"
             (format-time-string pomm-csv-history-file-timestamp-format)
             (symbol-name (alist-get 'status pomm-third-time--state))
             (symbol-name (alist-get 'kind (alist-get 'current pomm-third-time--state)))
             (or (alist-get 'iteration (alist-get 'current pomm-third-time--state)) 0)
             (pomm-third-time--break-time)
             (alist-get 'context pomm--state))
     nil pomm-third-time-csv-history-file 'append 1)))

(transient-define-prefix pomm-third-time-reset ()
  ["Are you sure you want to reset the Third Time timer?"
   ("y" "Yes" pomm-third-time--do-reset)
   ("n" "No" transient-quit-one)])

(defun pomm-third-time--dispatch-current-sound ()
  "Dispatch an appropriate sound for the current state of the timer."
  (cond
   ((eq (alist-get 'status pomm-third-time--state) 'stopped)
    (pomm--maybe-play-sound 'stop))
   ((eq (alist-get 'status pomm-third-time--state) 'running)
    (pomm--maybe-play-sound
     (alist-get 'kind (alist-get 'current pomm-third-time--state))))))

(defun pomm-third-time--calc-eval (value)
  "Evaluate VALUE and return number.

If VALUE is not a string, return it.

Otherwise, try to evaluate with `calc-eval'.  If unsuccessful, return
the calc error.  If the result is numeric, convert it to number and
return it, otherwise, return a value like a calc error."
  (if (stringp value)
      (let ((res (calc-eval value)))
        (if (listp res)
            res
          (if (string-match-p (rx (+ num) (? (: "." (* num)))) value)
              (string-to-number res)
            (list nil (format  "Can't parse number: %s" res)))))
    value))

(defun pomm-third-time--fraction ()
  "Get fraction of break time to work time."
  (let ((parsed (pomm-third-time--calc-eval
                 pomm-third-time-fraction)))
    (if (listp parsed)
        (user-error "Error in `pomm-third-time-fraction': %s" (nth 1 parsed))
      parsed)))

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

(defun pomm-third-time--total-time (&optional kind)
  "Get total time spent in `state` in the current iteration.

KIND is the same as in `pomm-third-time--state'."
  (let* ((kind (if kind kind 'work))
         (iteration (alist-get 'iteration
                               (alist-get 'current pomm-third-time--state)))
         (current-kind (alist-get 'kind (alist-get 'current pomm-third-time--state))))
    (apply
     #'+
     (if (eq current-kind kind)
         (pomm-third-time--current-period-time)
       0)
     (mapcar
      (lambda (item)
        (- (alist-get 'end-time item)
           (alist-get 'start-time item)))
      (seq-filter
       (lambda (item)
         (and (= (alist-get 'iteration item) iteration)
              (eq (alist-get 'kind item) kind)))
       (alist-get 'history pomm-third-time--state))))))


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
                                      (alist-get 'current pomm-third-time--state)))
        (current-context (alist-get 'context pomm-third-time--state)))
    (when current-kind
      (push `((kind . ,current-kind)
              (start-time . ,current-start-time)
              (end-time . ,(time-convert nil 'integer))
              (iteration . ,current-iteration)
              (context . ,current-context))
            (alist-get 'history pomm-third-time--state)))))

(defun pomm-third-time--format-period (seconds)
  "Format SECONDS into string."
  (if (>= seconds (* 60 60))
      (format-seconds "%.2h:%.2m:%.2s" seconds)
    (format-seconds "%.2m:%.2s" seconds)))

(defun pomm-third-time--dispatch-notification (kind)
  "Dispatch a notification about a start of a period.

KIND is the same as in `pomm-third-time--state'"
  (alert
   (pcase kind
     ('break (concat pomm-third-time-break-message
                     (format "\nTime available: %s"
                             (pomm-third-time--format-period
                              (pomm-third-time--break-time)))))
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
    (pomm-third-time--store-current-to-history)
    (setf (alist-get 'current pomm-third-time--state)
          `((kind . ,next-kind)
            (start-time . ,(time-convert nil 'integer))
            (break-time-bank . ,break-time)
            (iteration . ,iteration)))
    (pomm-third-time--dispatch-notification next-kind)
    (pomm-third-time--save-state)
    (run-hooks 'pomm-third-time-on-status-changed-hook)))

(defun pomm-third-time--on-tick ()
  "Function to execute on each timer tick."
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
    (setq pomm-third-time--timer (run-with-timer 0 1 'pomm-third-time--on-tick))))

(defun pomm-third-time--stop ()
  "Stop the running Third Time timer."
  (interactive)
  (unless (eq (alist-get 'status pomm-third-time--state) 'running)
    (user-error "The timer is not running!"))
  (pomm-third-time--store-current-to-history)
  (setf (alist-get 'status pomm-third-time--state) 'stopped
        (alist-get 'current pomm-third-time--state) nil
        (alist-get 'last-changed-time pomm-third-time--state)
        (time-convert nil 'integer))
  (run-hooks 'pomm-third-time-on-status-changed-hook)
  (when pomm-reset-context-on-iteration-end
    (setf (alist-get 'context pomm-third-time--state) nil)))

(transient-define-prefix pomm-third-time-stop ()
  ["This will reset the accumulated break time. Continue?"
   ("y" "Yes" pomm-third-time--stop)
   ("n" "No" transient-quit-one)])

(defun pomm-third-time-switch ()
  "Toggle work/break in the Third Time timer."
  (interactive)
  (pomm-third-time--switch))

(defun pomm-third-time-format-mode-line ()
  "Format the modeline string for the Third Time timer."
  (let ((current-status (alist-get 'status pomm-third-time--state)))
    (if (or (eq current-status 'stopped)
            (not (alist-get 'current pomm-third-time--state)))
        ""
      (let ((current-kind (alist-get 'kind (alist-get 'current pomm-third-time--state))))
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

(defun pomm-third-time-set-context ()
  "Set the current context for the Third Time timer."
  (interactive)
  (setf (alist-get 'context pomm-third-time--state)
        (prin1-to-string (read-minibuffer "Context: " (current-word)))))

(defun pomm-third-time--sync-org-clock ()
  "Sync org-clock with the pomodoro timer."
  (let* ((status (alist-get 'status pomm-third-time--state))
         (kind (alist-get 'kind (alist-get 'current pomm-third-time--state)))
         (active-p (and (eq kind 'work)
                        (eq status 'running)))
         (resume-next-time-p (not (eq status 'stopped))))
    (cond
     ((and active-p (not org-clock-current-task)
           pomm--sync-org-clock-was-stopped)
      (if pomm-org-clock-in-immediately
          (org-clock-in-last)
        (pomm--org-clock-in-last-after-action)))
     ((and (not active-p) org-clock-current-task)
      (org-clock-out)
      (setq pomm--sync-org-clock-was-stopped resume-next-time-p)))))

;;;###autoload
(defun pomm-third-time-start-with-context ()
  "Prompt for context call call `pomm-third-time-start'."
  (interactive)
  (pomm-third-time-set-context)
  (pomm-third-time-start))

;;;; Transient
(defun pomm-third-time--completing-read-calc ()
  "Do `completing-read' with `calc-eval'."
  (let ((res (completing-read
              "Time: "
              (lambda (string _ flag)
                (when (eq flag 'metadata)
                  (let ((res (pomm-third-time--calc-eval string)))
                    (if (listp res)
                        (message (nth 1 res))
                      (message "%f" res))))))))
    (let ((eval-res (pomm-third-time--calc-eval res)))
      (if (listp eval-res)
          (user-error "Bad value: %s" (nth 1 eval-res))
        res))))

(transient-define-infix pomm-third-time--set-fraction ()
  :class 'transient-lisp-variable
  :variable 'pomm-third-time-fraction
  :key "-f"
  :description "Fraction of break time to work time:"
  :reader (lambda (&rest _)
            (let ((current-value pomm-third-time-fraction))
              (condition-case error
                  (pomm-third-time--completing-read-calc)
                (error (progn
                         (message "%s" error)
                         current-value))))))

(transient-define-infix pomm-third-time-set-reset-context-on-iteration-end ()
  :class 'pomm--transient-lisp-variable-switch
  :variable 'pomm-reset-context-on-iteration-end
  :argument "--context-reset"
  :key "-r"
  :description "Reset the context on the interation end")

(defclass pomm-third-time--set-context-infix (transient-variable)
  ((transient :initform 'transient--do-call)
   (always-read :initform t)))

(cl-defmethod transient-init-value ((_ pomm-third-time--set-context-infix))
  "Initialize the value of context infix from `pomm-third-time-state'."
  (alist-get 'context pomm-third-time--state))

(cl-defmethod transient-infix-set ((_ pomm-third-time--set-context-infix) value)
  "Update `pomm-third-time-start' with VALUE from the context infix."
  (setf (alist-get 'context pomm-third-time--state) value))

(cl-defmethod transient-prompt ((_ pomm-third-time--set-context-infix))
  "Return the prompt text for the context infix."
  "Set context: ")

(cl-defmethod transient-format-value ((_ pomm-third-time--set-context-infix))
  "Format value for the context infix."
  (propertize (if-let (val (alist-get 'context pomm-third-time--state))
                  (prin1-to-string val)
                "unset")
              'face 'transient-value))

(transient-define-infix pomm-third-time--set-context ()
  :class 'pomm-third-time--set-context-infix
  :key "-c"
  :description "Context:")

(defclass pomm-third-time--transient-current (transient-suffix)
  ((transient :initform t))
  "A transient class to display the current state of the timer.")

(cl-defmethod transient-init-value ((_ pomm-third-time--transient-current))
  "A dummy method for `pomm-third-time--transient-current'.

The class doesn't actually have any value, but this is necessary for transient."
  nil)

(defun pomm-third-time--get-kind-face (kind)
  "Get a face for a KIND of period.

KIND is the same as in `pomm-third-time--state'"
  (pcase kind
    ('work 'success)
    ('break 'error)))

(cl-defmethod transient-format ((_ pomm-third-time--transient-current))
  "Format the state of the Third Time timer."
  (let ((status (alist-get 'status pomm-third-time--state)))
    (if (or (eq 'stopped status) (not (alist-get 'current pomm-third-time--state)))
        "The timer is not running"
      (let ((kind (alist-get 'kind (alist-get 'current pomm-third-time--state)))
            (start-time (alist-get 'start-time
                                   (alist-get 'current pomm-third-time--state)))
            (iteration (alist-get 'iteration
                                  (alist-get 'current pomm-third-time--state)))
            (break-time (pomm-third-time--break-time))
            (period-time (pomm-third-time--current-period-time))
            (total-work (pomm-third-time--total-time 'work))
            (total-break (pomm-third-time--total-time 'break)))
        (concat
         (format "Iteration #%d. " iteration)
         "State: "
         (propertize
          (upcase (symbol-name kind))
          'face
          (pomm-third-time--get-kind-face kind))
         ". Time: "
         (propertize
          (pomm-third-time--format-period period-time)
          'face 'success)
         " (started at "
         (propertize
          (format-time-string "%H:%M:%S" (seconds-to-time start-time))
          'face 'success)
         ")\nAvailable break time: "
         (propertize
          (pomm-third-time--format-period break-time)
          'face 'success)
         ".\nTotal time worked: "
         (propertize
          (pomm-third-time--format-period total-work)
          'face 'success)
         ". Total time braked: "
         (propertize
          (pomm-third-time--format-period total-break)
          'face 'success)
         ". Total time tracked: "
         (propertize
          (pomm-third-time--format-period (+ total-work total-break))
          'face 'success))))))

(defclass pomm-third-time--transient-history (transient-suffix)
  ((transient :initform t))
  "A transient class to display the history of the pomodoro timer.")

(cl-defmethod transient-init-value ((_ pomm-third-time--transient-history))
  "A dummy method for `pomm-third-time--transient-history'.

The class doesn't actually have any value, but this is necessary for transient."
  nil)

(cl-defmethod transient-format ((_ pomm-third-time--transient-history))
  "Format the history list for the transient buffer."
  (if (not (alist-get 'history pomm-third-time--state))
      "No history yet"
    (let ((previous-iteration 1000))
      (mapconcat
       (lambda (item)
         (let ((kind (alist-get 'kind item))
               (iteration (alist-get 'iteration item))
               (start-time (alist-get 'start-time item))
               (end-time (alist-get 'end-time item))
               (context (alist-get 'context item)))
           (concat
            (if (< iteration previous-iteration)
                (let ((is-first (= previous-iteration 1000)))
                  (setq previous-iteration iteration)
                  (if is-first
                      ""
                    "\n"))
              "")
            (format "[%02d] " iteration)
            (propertize
             (format "%12s  " (upcase (symbol-name kind)))
             'face (pomm-third-time--get-kind-face kind))
            (format-time-string "%H:%M" (seconds-to-time start-time))
            "-"
            (format-time-string "%H:%M" (seconds-to-time end-time))
            (if context
                (format " : %s" (propertize context 'face 'transient-value))
              ""))))
       (alist-get 'history pomm-third-time--state)
       "\n"))))

(transient-define-infix pomm-third-time--transient-current-suffix ()
  :class 'pomm-third-time--transient-current
  :key "~~2")

(transient-define-infix pomm-third-time--transient-history-suffix ()
  :class 'pomm-third-time--transient-history
  :key "~~1")

(transient-define-prefix pomm-third-time-transient ()
  ["Timer settings"
   (pomm-third-time--set-fraction)]
  ["Context settings"
   (pomm-third-time--set-context)
   (pomm-third-time-set-reset-context-on-iteration-end)]
  ["Commands"
   :class transient-row
   ("s" "Start the timer" pomm-third-time-start :transient t)
   ;; XXX I tried to use the `:if' predicate here, but unfortunately
   ;; visibilty doesn't refresh with `:transient t'
   ("S" "Stop the timer" pomm-third-time-stop :transient t)
   ("b" "Switch work/break" pomm-third-time-switch :transient t)
   ("R" "Reset" pomm-third-time-reset :transient t)
   ("u" "Update" pomm--transient-update :transient t)
   ("q" "Quit" transient-quit-one)]
  ["Status"
   (pomm-third-time--transient-current-suffix)]
  ["History"
   (pomm-third-time--transient-history-suffix)])

;;;###autoload
(defun pomm-third-time ()
  "Implementation of the Third Time timer in Emacs.

The idea of the technique is as follows:
- Work as long as you need, take a break as 1/3 of the work time (the
  fraction of work time to break time is set in
  `pomm-third-time-fraction')
- If you've ended a break early, unused break time is saved and added
  to the next break within the same session.
- If you've finished the session, either to take a longer break or to
  end working, remaining break time is discarded.  Each session starts
  from a clean slate.

The timer can have two states:
- Stopped.
  Can be started with \\='s' or `pomm-third-time-start'.
- Running.
  Can be stopped with \\='S' or `pomm-third-time-stop'.

If the timer is running, the current period type (work or break) can
be switched by \\='b' or `pomm-third-time-switch'.  If the break time
runs out, the timer automatically switches to work.

The timer supports setting \"context\", for example, a task on which
you\\='re working on.  It can be set with \\='-c' or
`pomm-third-time-set-context'.  This is useful together with CSV
logging, which is enabled if `pomm-third-time-csv-history-file' is
non-nil.

Enable `pomm-mode-line-mode' to display the timer state in the
modeline."
  (interactive)
  (unless pomm-third-time--state
    (pomm-third-time--init-state))
  (call-interactively #'pomm-third-time-transient))

(provide 'pomm-third-time)
;;; pomm-third-time.el ends here
