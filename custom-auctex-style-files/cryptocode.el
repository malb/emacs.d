;;; cryptocode.el --- AucTeX style file for the cryptocode package -*-emacs-lisp-*-
;;
;; Registers the environments defined by cryptocode.sty. Most use
;; \NewEnviron or \newenvironmentx with optional/required arguments,
;; which AucTeX's auto-parser does not always detect correctly.

(TeX-add-style-hook
 "cryptocode"
 (lambda ()
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

   (TeX-add-symbols
    ;; Block-level
    '("pseudocode"      [ "keys" ] "code")
    '("procedure"       [ "keys" ] "name/args" "code")
    '("pseudocodeblock" [ "keys" ] "code")
    '("procedureblock"  [ "keys" ] "name" "code")
    '("gameprocedure"   [ "keys" ] "code")
    ;; Structural
    '("pcind"           [ "level" ])
    '("pccomment"       [ "sep" ] "text")
    '("pclinecomment"   [ "sep" ] "text")
    '("pcrepeat"        "count")
    '("pcrepeatuntil"   "cond1" "cond2")
    ;; Messages
    '("sendmessage"     "path" "keys")
    '("sendmessageright" [ "path" ] "keys")
    '("sendmessageleft"  [ "path" ] "keys")
    ;; Nodes/boxes
    '("pcnode"          "name" [ "keys" ])
    '("pcdraw"          "tikz-code" [ "tikz-options" ])
    '("gamechange"      [ "color" ] "text")
    ;; Black-box
    '("bbrinput"        "value" [ "keys" ])
    '("bbroutput"       "value" [ "keys" ])
    '("bbrloop"         "node1" "node2" "keys")
    ;; Game proofs
    '("describegame"    [ "keys" ])
    '("addgamehop"      "i" "j" "keys")
    '("addstartgamehop" [ "startnr" ] "keys")
    '("addendgamehop"   [ "endnr" ] "keys")
    '("addloopgamehop"  [ "nr" ] "keys"))

   ;; Highlight cryptocode control keywords buffer-locally
   (font-lock-add-keywords
    nil `((,(rx "\\pc" (and (or "if" "then" "else" "elseif"
                               "for" "do" "while"
                               "return" "abort" "continue"
                               "const"
                               "and" "or"
                               "linecomment" "comment" "assert" "in")
                           symbol-end))
           0 'font-latex-bold-face prepend))
    'end)

   (font-latex-add-keywords '(("gameheading" "{")) 'sectioning-5)))



;; --- cryptocode.el ends here
