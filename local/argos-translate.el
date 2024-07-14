;;; argos-translate --- Translate text with argos-translate
;;;
;;; See https://github.com/argosopentech/argos-translate
;;;
;;; Commentary:
;;; Code:

(require 'guess-language)
(require 's)

(defgroup argos-translate nil
  "Local machine translation."
  :group 'tools)

(defcustom argos-translate-default-target-language "en"
  "Default output language."
  :group 'argos-translate
  :type 'string)

(defcustom argos-translate-default-source-language "de"
  "Default input language."
  :group 'argos-translate
  :type 'string)

(defcustom argos-translate-buffer-name "*argos-translate*"
  "Output buffer name."
  :group 'argos-translate
  :type 'string)

(defcustom argos-translate-command
  "argos-translate -f ${from} -t ${to} ${text}"
  "Command line command to call."
  :group 'argos-translate
  :type 'string)

(define-derived-mode argos-translate-mode text-mode "⇛"
  "Major mode for outputting argos translations.")

(defun argos-translate-guess-language (text &optional default-language)
  "Guess the language of TEXT and fall back to DEFAULT-LANGUAGE."
  (if (length< text guess-language-min-paragraph-length)
      (or default-language argos-translate-default-source-language)
    (with-temp-buffer
      (insert text)
      (replace-regexp-in-string
       "_" "-"
       (car (split-string
             (cadr (assq (guess-language-buffer) guess-language-langcodes))
             "_"))))))

(defun argos-translate-string (text &optional to from)
  "Translate TEXT from language FROM to language TO."
  (let* ((from (or from (argos-translate-guess-language text)))
         (to (or to
                 (cond
                  ((s-equals-p from argos-translate-default-target-language)
                   argos-translate-default-source-language)
                  (t
                   argos-translate-default-target-language)
                  )))
         (escaped-text (shell-quote-argument text))
         (cmd (s-format argos-translate-command 'aget
                        `(("to" . ,to)
                          ("from" . ,from)
                          ("text" . ,escaped-text))))
         (buffer (get-buffer-create argos-translate-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (argos-translate-mode)
      (goto-char (point-min)))
    (make-process
     :name "argos-translate"
     :buffer buffer
     :command (list shell-file-name shell-command-switch cmd)
     :sentinel (lambda (proc change)
                 (when (string-match "\\(finished\\|exited\\)" change)
                   (with-selected-window (get-buffer-window (process-buffer proc))
                     (goto-char (point-min))))))
    (display-buffer buffer)))

;;;###autoload
(defun argos-translate-region nil
  "Translate selected region or sentence at point."
  (interactive)
  (let* ((beg (if (not (use-region-p))
                  (save-excursion (backward-paragraph) (+ (point) 1))
                (region-beginning)))
         (end (if (not (use-region-p))
                  (save-excursion (forward-paragraph) (- (point) 1))
                (region-end)))
         (text (buffer-substring-no-properties beg end)))
    (argos-translate-string text)))

(provide 'argos-translate)
;;; argos-translate.el ends here
