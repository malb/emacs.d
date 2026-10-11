;;; iacrcc.el --- AUCTeX style for the IACR CoC submission class -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; iacrcc.cls loads most of its packages with \RequirePackage inside the
;; class file (often deep in \AtEndPreamble), which AUCTeX's self-parser
;; never sees: it only inspects \usepackage in the main .tex buffer.
;;
;; This style file re-runs AUCTeX's *built-in* styles for every package the
;; class loads, so documents using \documentclass{iacrcc} get proper
;; fontification, argument parsing, RefTeX support, and label/citation
;; completion for all of them (not just cleveref).
;;
;; Only packages that ship an AUCTeX style file are listed; the rest
;; (currfile, xkeyval, xstring, pgfkeys, xpatch, alphalph, tokcycle,
;; listofitems, xurl, totpages, luatex85, doclicense, float, lmodern, ...)
;; have no built-in style and are therefore omitted.
;;
;; If you compile with \documentclass[biblatex]{iacrcc}, add "biblatex"
;; (and drop "natbib"): the default (no option) uses natbib-style \cite.

(require 'tex)

(TeX-add-style-hook
 "iacrcc"
 '(lambda ()
    (TeX-run-style-hooks
     ;; --- math ---
     "amsmath" "amssymb" "amsthm" "mathtools"
     ;; --- low-level infrastructure / conditionals ---
     "expl3" "etoolbox" "iftex"
     ;; --- page layout, floats & typography ---
     "fontenc" "geometry" "afterpage" "caption" "floatrow"
     "fancyhdr" "graphicx" "microtype"
     ;; --- graphics & verbatim ---
     "tikz" "fancyvrb" "lineno"
     ;; --- citations (iacrcc default is natbib; swap for biblatex) ---
     "csquotes" "natbib"
     ;; --- hyperref is loaded at end-preamble: keep it late ---
     "hyperref"
     ;; --- cleveref must load after hyperref ---
     "cleveref")))

;; --- iacrcc.el ends here
