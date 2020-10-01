(require 'solarized-palettes)

(solarized-create-theme-file 'dark 'solarized-zenburn-malb
  solarized-zenburn-color-palette-alist
  `((custom-theme-set-faces
     theme-name

     `(org-block-begin-line ((t (:slant normal :foreground ,s-base0 :background ,s-base02 :inherit (org-meta-line) :extend t))))
     `(org-block-end-line   ((t (:slant normal :foreground ,s-base0 :background ,s-base02 :inherit (org-meta-line) :extend t))))
     `(org-block ((t (:foreground ,s-base0 :background ,(solarized-color-blend "#222222" s-base03 0.2) :extend t))))
     `(markdown-code-face ((t (:background ,(solarized-color-blend "#222222" s-base03 0.2) :inherit nil  :extend t))))
     `(markdown-language-keyword-face ((t (:background ,(solarized-color-blend "#222222" s-base03 0.2)
                                                       :inherit (highlight)))))
     `(stripe-highlight ((t (:background ,s-base02 :extend t))))
     `(helm-ff-file-extension ((t ())))
     )

    (custom-theme-set-variables theme-name
                                `(company-quickhelp-color-background ,s-base2)
                                `(company-quickhelp-color-foreground ,s-base00)
                                `(pdf-view-midnight-colors '(,s-base2 . ,s-base03))
                                `(org-todo-keyword-faces
                                  '(("CANCELLED" :foreground "gray" :weight bold)
                                    ("ONHOLD"    :foreground "gray" :weight bold)
                                    ("STALLED"   :foreground "gray" :weight bold)
                                    ("SUBMITTED" :foreground "gray" :weight bold)
                                    ("DELEGATED" :foreground "dark orange" :weight bold)
                                    ("WAIT"      :foreground "dark orange" :weight bold)
                                    ("COAUTHOR"  :foreground "dark orange" :weight bold))))
    t))
