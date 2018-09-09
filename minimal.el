;;; init.el --- minimal personal emacs config file -*- lexical-binding: t -*-
(setq inhibit-startup-screen t)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq-default mode-line-format nil)

(winner-mode 1)

(global-set-key (kbd "¬") #'other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq-default save-place t
              save-place-file (locate-user-emacs-file "places" ".emacs-places"))

(setq global-auto-revert-non-file-buffers t
      global-auto-revert-ignore-modes '(pdf-view-mode)
      auto-revert-verbose nil)

(global-auto-revert-mode 1)

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(defun malb/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'malb/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key (kbd "M-q") #'malb/fill-or-unfill)

(setq sentence-end-double-space nil)

(setq select-enable-clipboard t             ; use clipboard for copy and paste
      save-interprogram-paste-before-kill t ; keep a copy of clipboard stuff around
      mouse-yank-at-point t
      mouse-drag-copy-region t
      select-enable-primary t
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defvar malb/autosave-dir
  (expand-file-name "autosaves" user-emacs-directory))

(make-directory malb/autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat malb/autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

(defvar backup-dir (expand-file-name "autosaves" user-emacs-directory))
(setq backup-directory-alist (list (cons "." backup-dir)))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq current-language-environment "UTF-8")
(setq default-input-method "rfc1345")

(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq kill-whole-line t)

(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

(setq history-length 128)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)

(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(savehist-mode t)

(setq load-prefer-newer t)

(setq scroll-conservatively 10
      scroll-preserve-screen-position t)

(setq echo-keystrokes 0.1)

(setq set-mark-command-repeat-pop t)

(setq search-default-mode 'char-fold-to-regexp)

(setq kill-read-only-ok t)

(setq initial-major-mode 'org-mode
      initial-scratch-message "\
This buffer is for notes you don't want to save, and for Lisp evaluation.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer.

#+BEGIN_SRC emacs-lisp

#+END_SRC

")
