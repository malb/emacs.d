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

(TeX-add-style-hook
 "comments-bugs-todos"
 (lambda ()
   (TeX-add-symbols
    '("cmnt"    "author" "comment" [ "highlight" ])
    '("cmnt*"   "author" "comment" [ "highlight" ])
    '("todo"    "note" [ "highlight" ])
    '("todo*"   "note" [ "highlight" ])
    '("bug"     "note" [ "highlight" ])
    '("bug*"    "note" [ "highlight" ])
    '("hltodo"  "text")
    '("hlcmnt"  "text")
    '("hlbug"   "text"))

   (font-latex-add-keywords '(("cmnt" "*{{[")
                              ("todo" "*[{[")
                              ("bug"   "*{[")
                              ("hltodo" "{")
                              ("hlcmnt" "{")
                              ("hlbug"  "{"))
                            'function)))

;; --- comments-bugs-todos.el ends here
