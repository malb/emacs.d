;; -*- coding: utf-8; -*-
;;; empx.el --- Track cursor movement and forward/backward navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;; Author: ISouthRain
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: cursor, idle, timer
;; URL: https://github.com/ISouthRain/empx

;;; Commentary:
;;
;; This is to record where the cursor has stayed, to facilitate forward and backward navigation. This package is implemented by borrowing the function of xref.
;; How to Use: Enable this `xmap-mode`
;;             M-x xref-go-back OR xref-go-forward

;;; Code:
(require 'xref)

(defcustom empx-idle-time 5
  "Empx marker idle time in seconds."
  :type 'number
  :group 'empx)

(defcustom empx-target-functions
  '(find-file
    project-find-file
    project-switch-to-buffer
    goto-line
    avy-goto-line
    consult-buffer
    consult-outline
    consult-imenu
    consult-imenu-multi
    consult-ripgrep
    consult-line
    consult-line-multi)
  "List of functions to advise to push the current point marker onto the xref marker stack.
This will record the current cursor point before executing these functions."
  :type '(repeat function)
  :group 'empx)

(defvar empx--idle-timer nil
  "Timer to track idle time after cursor movement, marks the remaining time at the current cursor point.")

(defun empx--push-marker-stack (&rest _)
  "Push the current point marker onto the xref marker stack."
  (xref-push-marker-stack (point-marker)))

(defun empx--idle-function ()
  "Execute function if no actions occur within `empx-idle-time` seconds."
  (when (not (minibufferp))
    (empx--push-marker-stack)
    )
  )

(defun empx--reset-timer ()
  "Reset the idle timer after every action."
  (when empx--idle-timer
    (cancel-timer empx--idle-timer))
  (setq empx--idle-timer
        (run-with-idle-timer empx-idle-time nil #'empx--idle-function)))

(defun empx-enable-advice ()
  "Add advice to specified functions to push markers onto the xref stack."
  (dolist (target empx-target-functions)
    (advice-add target :before #'empx--push-marker-stack)))

(defun empx-mode-enable ()
  "Enable empx mode to track cursor movement and idle time."
  (add-hook 'post-command-hook #'empx--reset-timer)
  (empx-enable-advice))

(defun empx-mode-disable ()
  "Disable empx mode."
  (remove-hook 'post-command-hook #'empx--reset-timer)
  (when empx--idle-timer
    (cancel-timer empx--idle-timer)
    (setq empx--idle-timer nil))
  (dolist (target empx-target-functions)
    (advice-remove target #'empx--push-marker-stack)))

;;;###autoload
(define-minor-mode empx-mode
  "Empx records the cursor position for forward/backward navigation."
  :lighter " Empx"
  :global t
  (if empx-mode
      (empx-mode-enable)
    (empx-mode-disable)))

(provide 'empx)
