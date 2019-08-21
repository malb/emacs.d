;;; Exporting
(add-hook 'org-export-before-parsing-hook #'malb/ox-inline-comment)

(defun malb/ox-inline-comment (backend)
  (when (member backend '(latex odt))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "❰\\(?3:\\[[^\]]+\\] \\)?\\(?1:[^❱❙]+\\)\\(?:❙\\(?3:\\[[^\]]+\\] \\)?\\(?2:[^❱]+\\)\\)?❱"
              nil t)
        (replace-match
         (cond
          ((eq 'latex backend)
           (let ((author (if (match-string 3)
                             (format "[author=%s]" (substring (match-string 3) 1 -2))
                           "")))
             (if (match-string 2)
                 (format "@@latex:\\fxnote*%s{%s}{%s}@@"
                         author (match-string 2) (match-string 1))
               (format "@@latex:\\fxnote%s{%s}@@" author (match-string 1)))))
          ((eq 'odt backend)
           (format (if (match-string 2)
                       (let ((an-name (concat "__Annot_" (number-to-string (random)))))
                         (format "@@odt:<office:annotation office:name=\"%s\"><dc:creator>%%s</dc:creator><dc:date>%%s</dc:date><text:list><text:list-item><text:p>%s</text:p></text:list-item></text:list></office:annotation>%s<office:annotation-end office:name=\"%s\"/>@@"
                                 an-name
                                 (match-string 2)
                                 (match-string 1)
                                 an-name))
                     (format "@@odt:<office:annotation><dc:creator>%%s</dc:creator><dc:date>%%s</dc:date><text:list><text:list-item><text:p>%s</text:p></text:list-item></text:list></office:annotation>@@"
                             (match-string 1)))
                   (if (match-string 3) (substring (match-string 3) 1 -2) (user-full-name))
                   "no date")))
         nil t)))))


;;; Inserting
(defun malb/org-insert-inline-comment (arg)
  (interactive "P")
  (if (use-region-p)
      (let ((beg (min (point) (mark))) 
            (end (max (point) (mark))))
        (goto-char beg)
        (insert "❰")
        (goto-char (1+ end))
        (insert (concat"❙"
                       (when arg (malb/org-inline-comment-name))
                       "❱"))
        (backward-char))
    (insert (concat "❰"
                    (when arg (malb/org-inline-comment-name))
                    "❱"))
    (backward-char)))

(defvar malb/org-inline-comment-name-history nil)
(defun malb/org-inline-comment-name ()
  (concat
   "["
   (helm :sources '(malb/org-inline-comment-names-source
                    malb/org-inline-comment-names-fallback-source)
         :buffer "*aj helm choose oic-names*"
         :resume 'noresume
         :history 'malb/org-inline-comment-name-history)
   ;; (helm-comp-read
   ;;  "Författare: "
   ;;  malb/org-insert-inline-comment-name-history
   ;;  :input-history 'malb/org-insert-inline-comment-name-history
   ;;  :name "Comment name" :buffer "*oic-helm*")
   "] "))


(defvar malb/org-inline-comment-names-source
  (helm-build-sync-source "Inline comment names"
    :candidates 'malb/org-inline-comment-name-history
    :fuzzy-match t
    :action (helm-make-actions "Insert" 'identity "Delete" 'malb/org-inline-comment-remove-name)
    :persistent-action 'malb/org-inline-comment-remove-name
    :multiline t)
  "Source for inline comment names")

(defvar malb/org-inline-comment-names-fallback-source
  '((name . "Insert")
    (dummy)
    (action . (("insert" . identity)))))

(defun malb/org-inline-comment-remove-name (_cand)
  (let ((marked (helm-marked-candidates)))
    (dolist (el marked)
      (setq malb/org-inline-comment-name-history
            (delete el malb/org-inline-comment-name-history))))
  (helm-force-update))

(font-lock-add-keywords 'org-mode '(("❰\\(\\[[^\]]+\\] \\)?\\([^❱❙]+\\)❱"
                                     (1 'bold prepend t)
                                     (2 'helm-buffer-process prepend))
                                    ("❰\\([^❱❙]+\\)❙\\(\\[[^\]]+\\] \\)?\\([^❱]+\\)❱"
                                     (1 'org-target prepend)
                                     (2 'bold prepend t)
                                     (3 'helm-buffer-process prepend))))
