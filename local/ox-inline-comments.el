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
                             (format "\textbf{%s}: " (substring (match-string 3) 1 -2))
                           "")))
             (if (match-string 2)
                 (format "@@latex:\\todo[]{%s\emph{%s} %s}@@"
                         author (match-string 2) (match-string 1))
               (format "@@latex:\\todo[]{%s%s}@@" author (match-string 1)))))
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
(defun malb/org-insert-inline-comment ()
  (interactive)
  (if (use-region-p)
      (let ((beg (min (point) (mark))) 
            (end (max (point) (mark))))
        (goto-char beg)
        (insert "❰")
        (goto-char (1+ end))
        (insert (concat"❙" "❱"))
        (backward-char))
    (insert (concat "❰" "❱"))
    (backward-char)))

(font-lock-add-keywords 'org-mode '(("❰\\(\\[[^\]]+\\] \\)?\\([^❱❙]+\\)❱"
                                     (1 'bold prepend t)
                                     (2 'helm-buffer-process prepend))
                                    ("❰\\([^❱❙]+\\)❙\\(\\[[^\]]+\\] \\)?\\([^❱]+\\)❱"
                                     (1 'org-target prepend)
                                     (2 'bold prepend t)
                                     (3 'helm-buffer-process prepend))))
