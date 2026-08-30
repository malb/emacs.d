;;; llncs.el --- AucTeX style file for the Springer LNCS class -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Registers the theorem-like environments defined by llncs.cls, which
;; uses the proprietary \spn@wtheorem macro (invisible to AucTeX's
;; auto-parser).
;;
;; Numbered environments get LaTeX-env-label (auto-number + labelable);
;; unnumbered ones get LaTeX-env-text.

(TeX-add-style-hook
 "llncs"
 (lambda ()
   (LaTeX-add-environments
    ;; Numbered
    '("theorem"    LaTeX-env-label)
    '("lemma"      LaTeX-env-label)
    '("corollary"  LaTeX-env-label)
    '("definition" LaTeX-env-label)
    '("example"    LaTeX-env-label)
    '("proposition" LaTeX-env-label)
    '("remark"     LaTeX-env-label)
    '("note"       LaTeX-env-label)
    '("case"       LaTeX-env-label)
    '("conjecture" LaTeX-env-label)
    '("exercise"   LaTeX-env-label)
    '("problem"    LaTeX-env-label)
    '("property"   LaTeX-env-label)
    '("question"   LaTeX-env-label)
    '("solution"   LaTeX-env-label)
    ;; Unnumbered
    '("claim"      LaTeX-env-text)
    '("proof"      LaTeX-env-text))))

;; --- llncs.el ends here
