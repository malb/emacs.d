;;; cryptocode.el --- AucTeX style file for the cryptocode package -*-emacs-lisp-*-
;;
;; Registers the environments defined by cryptocode.sty. Most use
;; \NewEnviron or \newenvironmentx with optional/required arguments,
;; which AucTeX's auto-parser does not always detect correctly.

(LaTeX-add-environments
 ;; Simple, no arguments
 '("pccenter"       LaTeX-env-text)
 '("subprocedure"   LaTeX-env-text)
 '("pcimage"        LaTeX-env-text)
 ;; Pseudocode stacks — optional key-value
 '("pchstack"       LaTeX-env-args ["pcstackkeys"])
 '("pcvstack"       LaTeX-env-args ["pcstackkeys"])
 ;; Black-box environments
 '("bbrenv"         LaTeX-env-args ["bbrenvkeys"] "boxname" ["skip"])
 '("bbrbox"         LaTeX-env-args ["bbrboxkeys"])
 '("bbroracle"      LaTeX-env-args "oracle-name" ["bbroraclekeys"])
 '("bbrchallenger"  LaTeX-env-args "challenger-name" ["bbrchallengerkeys"])
 '("bbrpic"         LaTeX-env-args ["tikzoptions"])
 ;; Game proofs
 '("gameproof"      LaTeX-env-args ["pcgameproofkeys"])
 '("gamedescription" LaTeX-env-args ["pcgameproofkeys"]))

;; Highlight cryptocode control keywords buffer-locally
(font-lock-add-keywords
 nil `((,(rx "\\pc" (and (or "if" "then" "else" "elseif"
                            "for" "do"
                            "return" "abort" "continue"
                            "const"
                            "and" "or"
                            "linecomment" "comment" "assert" "in")
                        symbol-end))
        0 'font-latex-bold-face prepend))
 'end)

;; --- cryptocode.el ends here
