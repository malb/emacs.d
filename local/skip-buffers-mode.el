;;; skip-buffers-mode --- skip boring next/previous-buffer
;;; Commentary:
;;; https://www.reddit.com/r/emacs/comments/b058f8/weekly_tipstricketc_thread/

;;; Code:

(require 'cl-lib)

(defvar skip-buffers-patterns
  '("*helm.**")
  "List of patterns that match buffers to ignore in `next-buffer'/`previous-buffer'.")

(defun skip-buffers--change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `skip-buffers-patterns'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (cl-some (lambda (pattern) (string-match-p pattern (buffer-name)))
                        skip-buffers-patterns)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun skip-buffers-next-buffer ()
  "Variant of `next-buffer' that skips buffers matching `skip-buffers-patterns'."
  (interactive)
  (skip-buffers--change-buffer 'next-buffer))

(defun skip-buffers-previous-buffer ()
  "Variant of `previous-buffer' that skips buffers matching `skip-buffers-patterns'."
  (interactive)
  (skip-buffers--change-buffer 'previous-buffer))

;;;###autoload
(define-minor-mode skip-buffers-mode
  "Skip buffers you don't want to see."
  :global t
  :lighter " skp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap next-buffer] 'skip-buffers-next-buffer)
            (define-key map [remap previous-buffer] 'skip-buffers-previous-buffer)
            map))

(provide 'skip-buffers-mode)
;;; skip-buffers-mode.el ends here
