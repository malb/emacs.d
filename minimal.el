;;; init.el --- minimal personal emacs config file -*- lexical-binding: t -*-
(setq inhibit-startup-screen t)

;; DISABLE CLUTTER
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(winner-mode 1)

(global-set-key (kbd "¬") #'other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer)


;; COMPLETION

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq-default save-place t
              save-place-file (locate-user-emacs-file "places" ".emacs-places"))

;; AUTO REVERT MODE

(require 'autorevert)
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

(setq sentence-end-double-space nil
      select-enable-clipboard t             ; use clipboard for copy and paste
      save-interprogram-paste-before-kill t ; keep a copy of clipboard stuff around
      mouse-yank-at-point t
      mouse-drag-copy-region t
      select-enable-primary t
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; AUTOSAVING
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

(require 'savehist)

(setq history-length 128
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

(savehist-mode t)


;; UTF-8

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq locale-coding-system 'utf-8
      current-language-environment "UTF-8"
      default-input-method "rfc1345")
(prefer-coding-system 'utf-8)

;; NO TABS
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq minibuffer-prompt-properties (quote (read-only nil
	                                                 cursor-intangible t
 	                                                 face minibuffer-prompt)))

(setq kill-whole-line t
      load-prefer-newer t
      scroll-conservatively 10
      scroll-preserve-screen-position t
      echo-keystrokes 0.1
      set-mark-command-repeat-pop t
      search-default-mode 'char-fold-to-regexp
      kill-read-only-ok t

      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; LONG LINES

(setq bidi-paragraph-direction 'left-to-right)

(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t)
  (global-so-long-mode 1))

;; UI

(when (display-graphic-p)
  (setq default-frame-alist
        (append (list
                 '(font . "Input 18")
                 ;; '(unsplittable . t)
                 ;; '(buffer-predicate . (lambda (x) nil))
	             '(min-height . 1)
                 '(height     . 42)
	             '(min-width  . 40)
	             '(width      . 74)
	             '(foreground-color . "#333333")
                 '(background-color . "#ffffff")
                 '(cursor-color . "black")
                 '(internal-border-width . 1)
                 '(vertical-scroll-bars . nil)
                 '(left-fringe . 32)
                 '(right-fringe . 32)
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))

  (set-face-attribute 'mode-line nil
                      :height 0.5
                      :foreground "#999977"
                      :background "#ffffff"
                      :overline nil
		              :underline nil
		              :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :height 0.5
                      :foreground "#999977"
                      :background "#ffffff"
                      :overline nil
                      :underline nil
		              :inherit nil
                      :box nil)
  (set-face-attribute 'header-line nil
		              :weight 'light
                      :foreground "#000000"
                      :background "#f0f0f0"
                      :overline nil
                      :underline nil
                      :box nil
                      :box `(:line-width 1 :color "#ffffff" :style nil)
		              :inherit nil)

  (set-face-attribute 'internal-border nil
                      :background "#777777")
  (set-face-attribute 'fringe nil
                      :foreground "#cccccc"
                      :background "#ffffff")
  (set-face-attribute 'bold nil
                      :weight 'medium))

(defun header-line-render (left right)
  (let* ((available-width (- (window-total-width) (length left) )))
    (format (format "%%s%%%ds" available-width) left right)))

(setq-default header-line-format
              '((:eval
                 (header-line-render
                  (format-mode-line
	               (list
	                (cond ((and buffer-file-name (buffer-modified-p)) " [M] ")
		                  (buffer-read-only                           " [RO] ")
		                  (t                                          " "))
	                (propertize "%b" 'face '(:weight regular))
	                " (%m)"
	                (propertize " " 'display '(raise +0.25))
	                (propertize " " 'display '(raise -0.30))))
                  (format-mode-line
	               (list "%l:%c "))))))

(setq-default mode-line-format "%-")

(defun update-buffers-mode-line ()
  (dolist (window (window-list))
    (with-selected-window window
      (if (or (one-window-p t)
	          (eq (window-in-direction 'below) (minibuffer-window))
	          (not (window-in-direction 'below)))
	      (with-current-buffer (window-buffer window)
	        (setq mode-line-format "%-"))
	    (with-current-buffer (window-buffer window)
 	      (setq mode-line-format nil)))
      (if (window-in-direction 'above)
	      (face-remap-add-relative 'header-line '(:overline "#777777"))
	    (face-remap-add-relative 'header-line '(:overline nil))))))
(add-hook 'window-configuration-change-hook 'update-buffers-mode-line)

(dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                      " *Minibuf-1*" " *Echo Area 1*"))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (face-remap-add-relative 'default '(:foreground "#999999")))))


(eval-after-load "auth-source"
  '(setq auth-source-save-behavior nil))
