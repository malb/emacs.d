;;; comments-bugs-todos.el --- AucTeX style file for comments-bugs-todos.sty -*-emacs-lisp-*-
;;
;; Registers the commands defined by comments-bugs-todos.sty, an
;; opinionated wrapper around the todonotes package.
;;
;; Signatures (l3doc):
;;   \cmnt{s m o}  star, author, comment, optional highlight
;;   \todo{s m o}  star, note, optional highlight
;;   \bug{s m o}   star, note, optional highlight
;;   \hlcmnt{m}    text highlighted in CommentColor
;;   \hltodo{m}    text highlighted in TaskColor
;;   \hlbug{m}     text highlighted in BugColor

(LaTeX-add-command "\\cmnt"   "author" "comment" ["highlight"])
(LaTeX-add-command "\\todo"   "note" ["highlight"])
(LaTeX-add-command "\\bug"    "note" ["highlight"])
(LaTeX-add-command "\\hltodo" "text")
(LaTeX-add-command "\\hlcmnt" "text")
(LaTeX-add-command "\\hlbug"  "text")

;; --- comments-bugs-todos.el ends here
