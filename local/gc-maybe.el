;;; gc-maybe.el --- GC Maybe Trick -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/gc-maybe
;; Version: 0.3
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; GC Maybe Trick.


;;; Code:

;;;; User options

(defgroup gc-maybe nil
  "Group for `gc-maybe' customizations."
  :group 'alloc)

(defcustom gc-maybe-cons-threshold gc-cons-threshold
  "Number of bytes of consing between garbage collections."
  :type 'integer)

(defcustom gc-maybe-cons-percentage gc-cons-percentage
  "Portion of the heap used for allocation."
  :type 'float)

(defcustom gc-maybe-cons-threshold-max (* 512 1024 1024)
  "Maximum number of bytes of consing between garbage collections."
  :type 'integer)

(defcustom gc-maybe-cons-percentage-max 0.6
  "Maximum portion of the heap used for allocation."
  :type 'float)

(defcustom gc-maybe-idle-delay 8
  "Idle time in seconds to run `gc-maybe' when `gc-maybe-mode' is on."
  :type 'integer)

(defcustom gc-maybe-idle-restore 5
  "Idle time in seconds to restore briefly raised GC values to defaults."
  :type 'integer)

(defcustom gc-maybe-verbose nil
  "Non-nil to display GC stats in echo area."
  :type 'boolean)

(defcustom gc-maybe-log-stats-in-buffer nil
  "Non-nil to log GC stats in `gc-maybe-log-buffer'."
  :type 'boolean)

(defcustom gc-maybe-log-buffer "*gc-log*"
  "GC log buffer."
  :type 'string)


;;;; Functions

;;;;; Set GC cons thresholds

(defun gc-maybe-lower-threshold (&rest _)
  "Lower GC cons threshold.
Set `gc-cons-threshold' and `gc-cons-percentage' to their standard values."
  (setq gc-cons-threshold  (* 800 1000)
        gc-cons-percentage 0.1))

(defun gc-maybe-restore-threshold (&rest _)
  "Restore GC cons threshold to default values."
  (setq gc-cons-threshold  gc-maybe-cons-threshold
        gc-cons-percentage gc-maybe-cons-percentage))

(defun gc-maybe-raise-threshold (&rest _)
  "Raise GC cons threshold."
  (setq gc-cons-threshold  gc-maybe-cons-threshold-max
        gc-cons-percentage gc-maybe-cons-percentage-max))

(defvar gc-maybe-restore-threshold-timer nil
  "Idle timer to restore GC threshold.")

(defun gc-maybe-raise-threshold-briefly (&rest _)
  "Raise GC cons threshold briefly.
Restore it after `gc-maybe-idle-restore' seconds."
  (gc-maybe-raise-threshold)
  (unless (member gc-maybe-restore-threshold-timer timer-idle-list)
    (cancel-function-timers 'gc-maybe-restore-threshold)
    (setq gc-maybe-restore-threshold-timer
          (run-with-idle-timer gc-maybe-idle-restore nil
                               #'gc-maybe-restore-threshold))))


;;;;; GC log

(defun gc-maybe-average ()
  "Return the average GC time."
  (condition-case nil
      (/ gc-elapsed gcs-done)
    (arith-error gc-elapsed)))

(defvar gc-maybe-last-gc-time (gc-maybe-average)
  "Duration of the last garbage collection.")

(defun gc-maybe-log-msg ()
  "Return the GC log message string."
  (concat
   (format-time-string "[%F %T] " (current-time))
   (format "GC took %.3fs, avg is %.3fs in %s GCs"
           gc-maybe-last-gc-time (gc-maybe-average) gcs-done)))

(defun gc-maybe-log-display ()
  "Display `gc-maybe-log-msg' in echo area."
  (message (gc-maybe-log-msg)))

(defmacro gc-maybe-with-log-buffer (&rest body)
  "Insert BODY in `gc-maybe-log-buffer'."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create gc-maybe-log-buffer)
     (special-mode)
     (setq-local buffer-read-only nil)
     (goto-char (point-max))
     ,@body
     (setq-local buffer-read-only t)))

;;;###autoload
(defun gc-maybe-log-current ()
  "Print current GC statistics."
  (interactive)
  (let ((msg
         (format
          "[GC] Cons threshold: %s / Percentage: %s / Avg: %.3fs / GCs: %s"
          (file-size-human-readable gc-cons-threshold 'iec " ")
          gc-cons-percentage
          (gc-maybe-average)
          gcs-done)))
    (when gc-maybe-log-stats-in-buffer
      (gc-maybe-with-log-buffer
        (insert (format "-----\n%s\n-----\n" msg))))
    (message msg)))

;;;###autoload
(defun gc-maybe-log-usage ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (gc-maybe-with-log-buffer
    (insert
     (concat
      "-----"
      (format "\n\n%-14s %-10s %-10s %-10s\n" 'TYPE 'USED 'FREE 'TOTAL)
      (make-string 50 ?-)
      (mapconcat
       (lambda (i)
         (let ((type (nth 0 i)) (size (nth 1 i))
               (used (nth 2 i)) (free (nth 3 i)))
           (format
            "\n%-14s %-10s %-10s %-10s"
            type
            (file-size-human-readable (* used size) 'iec " ")
            (file-size-human-readable (* (or free 0) size) 'iec " ")
            (file-size-human-readable (+ (* used size)
                                         (* (or free 0) size))
                                      'iec " "))))
       (garbage-collect))
      (concat "\n\n-----\n"))))
  (pop-to-buffer gc-maybe-log-buffer)
  (goto-char (point-max)))


;;;;; GC Maybe

(defmacro gc-maybe-time (&rest body)
  "Return the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     (and ,@body (float-time (time-since time)))))

(defun gc-maybe--run (gc-fn)
  "Run garbage collection function GC-FN."
  (when (setq gc-maybe-last-gc-time (gc-maybe-time (funcall gc-fn)))
    (and gc-maybe-log-stats-in-buffer
         (gc-maybe-with-log-buffer
           (insert (concat (gc-maybe-log-msg) "\n"))))
    (and gc-maybe-verbose
         (gc-maybe-log-display))))

;;;###autoload
(defun gc-maybe ()
  "Maybe GC with `garbage-collect-maybe' and 1/`gc-cons-percentage' factor."
  (gc-maybe--run
   (lambda ()
     (garbage-collect-maybe (round (/ 1 gc-cons-percentage))))))

;;;###autoload
(define-minor-mode gc-maybe-mode
  "Minor mode for GC strategy."
  :global t
  :lighter nil
  (if gc-maybe-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'gc-maybe-raise-threshold -90)
        (add-hook 'minibuffer-exit-hook  #'gc-maybe-restore-threshold 90)
        (run-with-idle-timer gc-maybe-idle-delay t #'gc-maybe))
    (remove-hook 'minibuffer-setup-hook #'gc-maybe-raise-threshold)
    (remove-hook 'minibuffer-exit-hook  #'gc-maybe-restore-threshold)
    (cancel-function-timers #'gc-maybe)))


(provide 'gc-maybe)

;;; gc-maybe.el ends here
