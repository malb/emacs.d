;;; verifpal-mode --- edit verifpal models.
;;; Commentary:

;;; Code:

(defgroup verifpal nil
  "Major mode for editing verifpal models.")

(defcustom verifpal-command "verifpal"
  "Command to run verifpal."
  :group 'verifpal
  :type '(choice (string :tag "Shell command") (repeat (string)) function))

(setq verifpal-highlights
      `((,(rx (or
               "attacker"
               "principal"
               "generates"
               "knows"
               "leaks"
               "queries"
               "ASSERT"
               "CONCAT"
               "SPLIT"
               "HASH"
               "MAC"
               "HKDF"
               "PW_HASH"
               "ENC"
               "DEC"
               "AEAD_ENC"
               "AEAD_DEC"
               "PKE_ENC"
               "PKE_DEC"
               "SIGNVERIFY"
               "SIGN"
               "RINGSIGNVERIFY"
               "RINGSIGN"
               "BLIND"
               "UNBLIND"
               "SHAMIR_SPLIT"
               "SHAMIR_JOIN"
               "->")) . font-lock-function-name-face)
        (,(rx (or
               "active"
               "passive"
               "authentication?"
               "confidentiality?"
               "freshness?"
               "unlinkability?"
               "G"
               ))
         . font-lock-constant-face)))

(defvar verifpal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?c)] 'verifpal-run-verify)
    map)
  "Keymap for `verifpal-mode'.")

(define-derived-mode verifpal-mode prog-mode "verifpal"
  "major mode for editing verifpal code.
  \\{verifpal-mode-map}
  "
  (kill-all-local-variables)
  (setq font-lock-defaults '(verifpal-highlights))
  (use-local-map verifpal-mode-map))

(defun verifpal-run-verify ()
  "Run verifpal verify."
  (interactive)
  (let ((command (concat verifpal-command " verify " (buffer-file-name))))
    (compilation-start command t)))

(add-to-list 'auto-mode-alist '("\\.vp" . verifpal-mode))

(provide 'verifpal-mode)
