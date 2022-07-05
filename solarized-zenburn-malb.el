(require 'solarized-palettes)

(solarized-create-theme-file 'dark 'solarized-zenburn-malb
  solarized-zenburn-color-palette-alist
  `((custom-theme-set-faces
     theme-name

     `(org-block-begin-line ((t (:slant normal :foreground ,s-base0 :background ,s-base02 :inherit (org-meta-line) :extend t))))
     `(org-block-end-line   ((t (:slant normal :foreground ,s-base0 :background ,s-base02 :inherit (org-meta-line) :extend t))))
     `(org-block ((t (:foreground ,s-base0 :background ,(solarized-color-blend "#222222" s-base03 0.2) :extend t))))
     `(org-agenda-structure
       ((,class (:foreground ,base1 :background ,base02
                             :weight bold :slant normal :inverse-video nil :height ,solarized-height-plus-1
                             :underline nil :extend t
                             :box (:line-width 2 :color ,base03)))))
     `(markdown-code-face ((t (:background ,(solarized-color-blend "#222222" s-base03 0.2) :inherit nil  :extend t))))
     `(markdown-language-keyword-face ((t (:background ,(solarized-color-blend "#222222" s-base03 0.2)
                                                       :inherit (highlight)))))
     `(hl-sentence ((t (:background ,orange-2bg :inherit (highlight)))))
     `(hl-line ((t (:background ,orange-2bg :inherit (highlight)))))
     `(mu4e-header-highlight-face ((t (:background ,orange-2bg :inherit (highlight)))))

     `(jupyter-repl-input-prompt ((t (:foreground ,green :weight bold))))
     `(jupyter-repl-output-prompt ((t (:foreground ,red :weight bold))))
     `(jupyter-repl-traceback ((t (:background ,red-1bg))))

     `(show-paren-match     ((,class (:foreground unspecified :background ,cyan-1bg :inherit (highlight) :extend t))))
     `(show-paren-mismatch  ((,class (:foreground ,base02 :background ,red :weight ,s-maybe-bold))))

     `(stripe-highlight ((t (:background ,s-base02 :extend t))))
     `(helm-ff-file-extension ((t ())))
     `(flycheck-fringe-error ((,class (:foreground ,(if solarized-emphasize-indicators red-hc red) :weight bold))))
     `(flycheck-fringe-warning ((,class (:foreground ,(if solarized-emphasize-indicators yellow-hc yellow) :weight bold))))
     `(flycheck-fringe-info ((,class (:foreground ,(if solarized-emphasize-indicators blue-hc base01) :weight bold))))
     `(mu4e-thread-folding-child-face         ((t (:background "#3a3a3a" :extend t))))
     `(mu4e-thread-folding-root-unfolded-face ((t (:background "#444444" :extend t))))
     `(mu4e-related-face ((t (:foreground ,s-base2))))
     `(jupyter-repl-input-prompt  ((t (:foreground ,green :weight bold))))
     `(jupyter-repl-output-prompt ((t (:foreground ,red   :weight bold)))))

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
