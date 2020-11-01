;;; latex-math-yank --- convenience functions for LaTeX yanking
;;; Commentary:
;;;
;;; 100% nicked from
;;; - https://abizjak.github.io/emacs/2016/03/05/latex-environment-yank.html
;;; - https://abizjak.github.io/emacs/2016/03/05/latex-environment.html

;;; Code:

(require 'dash)

(defun malb/first-nsp-after (p)
  "Go up to first non-whitespace character *after* the given position `P."
  (goto-char p)
  (skip-chars-forward " \n\r"))

(defun malb/first-nsp-before (p)
  "Go to just after the first non-whitespace char *before* the given position `P`."
  (goto-char p)
  (skip-chars-backward " \n\r"))


(defconst malb/math-display-delims
  '(("\\begin{align}" "\\end{align}")
    ("\\begin{align\*}" "\\end{align\*}")
    ("\\begin{displaymath}" "\\end{displaymath}")
    ("\\begin{mathpar}" "\\end{mathpar}")
    ("\\begin{equation}" "\\end{equation}")
    ("\\begin{equation*}" "\\end{equation*}")
    ("\\[" "\\]")
    ("\\(" "\\)"))
  "Delimiters of math mode, except $.")

(defun malb/latex-in-math-mode ()
  "Check if we are currently in math mode.
Uses`malb/math-display-delims' variable and relies on auctex to fontify
the math mode correctly."
  (interactive)
  (let ((cur (point)))
    (if (equal (char-after cur) 92) ;; if the character we are looking at is \ then we
        ;; might be looking at \end{...}. In this case we
        ;; must check if the previous character is in
        ;; font-latex-math-face.
        (malb/latex-check-if-math-face (get-text-property (max (- cur 1) (point-min)) 'face))
      ;; Otherwise we check as follows. The character we are looking at must be
      ;; in font-latex-math-face. But if we are looking at the opening $ then we must also
      ;; check the previous character, since $ is already in
      ;; font-latex-math-face.
      (and (malb/latex-check-if-math-face (get-text-property cur 'face))
           (if (equal (char-after cur) 36) ;; if the char we are looking at is $
               (malb/latex-check-if-math-face (get-text-property (max (- cur 1) (point-min)) 'face))
             t))
      )
    ))

(defun malb/latex-check-if-math-face (fp)
  "Check if `font-latex-math-face' is a face in `FP`."
  (cond
   ((symbolp fp) (equal fp 'font-latex-math-face))
   ((listp fp) (member 'font-latex-math-face fp))
   )
  )

(defun malb/latex-start-of-math-environment ()
  "Return a `nil' if we are not looking at the start of a math environment.
Otherwise it returns the length of the start delimiter,
e.g., 1 if $."
  (if (equal (char-after) 36) (list 1) ;; check if we are looking at $ first
    (-keep #'(lambda (pair)
               (when (looking-at-p (regexp-quote (first pair))) (length (first pair))))
           malb/math-display-delims)))

(defun malb/latex-end-of-math-environment ()
  "Return a `nil' if we are not looking at the end of a math environment.
Otherwise it returns the length of the end delimiter,
e.g., 1 if $."
  (if (equal (char-before) 36) (list 1) ;; check if we are just after $ first
    (-keep #'(lambda (pair)
               (save-excursion (ignore-errors ;; backward-char will signal an error if we try to go back too far
                                 (backward-char (length (second pair)))
                                 (when (looking-at-p (regexp-quote (second pair)))
                                   (length (second pair)))
                                 )))
           malb/math-display-delims)))

(defun malb/latex-remove-math-delims (str)
  "Remove math delimiters at the beginning and end of the given string `STR`.
There can be whitespace at the beginning and at the end of the
string. If it is, it is left there."
  (with-temp-buffer
    (insert str)
    (malb/first-nsp-after (point-min))
    (let ((x (malb/latex-start-of-math-environment)))
      (when x
        (delete-char (first x))
        ;; remove the newlines as well (in case there is a newline). This
        ;; works better when removing \begin{...}, since otherwise there is
        ;; redundant space left.
        (malb/remove-newline-forward)))
    (malb/first-nsp-before (point-max))
    (let ((x (malb/latex-end-of-math-environment)))
      (when x
        (delete-char (- (first x)))
        (malb/remove-newline-backward)))
    (buffer-string)
    )
  )

(defun malb/remove-newline-forward ()
  "This is technically incorrect, but correct in practice."
  (while (member (char-after) (list 10 13)) ;; 10 is \n, 13 is \r
    (delete-char 1)
    )
  )

(defun malb/remove-newline-backward ()
  "This is technically incorrect, but correct in practice."
  (while (member (char-before) (list 10 13)) ;; 10 is \n, 13 is \r
    (delete-char -1)
    )
  )

(defun malb/insert-for-yank/remove-math-delims (phi str)
  (funcall phi  (if (and (or (equal major-mode 'latex-mode)
                             (equal major-mode 'LaTeX-mode)
                             (equal major-mode 'org-mode))
                         (malb/latex-in-math-mode))
                    (malb/latex-remove-math-delims str)
                  str)))



(defun malb/latex-remove-math-delimiters (beg end)
  "Remove delimiters $ ... $, \[ ... \] and/or \( \) in an active region `BEG` and `END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (malb/first-nsp-after (point-min))
      (cond ((equal (char-after) (string-to-char "$")) (delete-char 1))
            ((looking-at-p "\\\\\\[") (delete-char 2))
            ((looking-at-p "\\\\(") (delete-char 2)))

      (malb/first-nsp-before (point-max))
      ;; if the last char is a comma or full stop try skipping it and clearing $
      ;; and \] before it
      ;; This is useful when the formula that was inlined and needs to be displayed
      ;; was at the end of the sentence or clause.
      (when (member (char-to-string (char-before)) '("." ","))
        (backward-char))
      (cond ((equal (char-before) (string-to-char "$")) (delete-char -1))
            ((looking-back "\\\\\\]" nil) (delete-char -2))
            ((looking-back "\\\\)" nil) (delete-char -2))))))

(defun malb/LaTeX-environment-menu-advice (env)
  (when (and (use-region-p)
             (or (member env (list "align"
                                   "align*"
                                   "displaymath"
                                   "equation"
                                   "equation*"))))
    (let ((deactivate-mark nil)) ;; in order to prevent deactivation
      ;; of transient-mark-mode. Relies
      ;; essentially on the fact that
      ;; `deactivate-mark' is dynamically
      ;; bound.
      (malb/latex-remove-math-delimiters (region-beginning) (region-end)))))

(advice-add 'insert-for-yank :around 'malb/insert-for-yank/remove-math-delims)
(advice-add 'LaTeX-environment-menu :before 'malb/LaTeX-environment-menu-advice)


;; to disable the above advice
;; (advice-remove 'insert-for-yank 'malb/insert-for-yank/remove-math-delims)

(provide 'latex-math-yank)
;;; latex-math-yank.el ends here
