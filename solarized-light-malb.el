(defvar solarized-light-malb-color-palette-alist
  '(;; solarized-light palette
    (base03      . "#002b36")
    (base02      . "#073642")
    (base01      . "#586e75")
    (base00      . "#657b83")
    (base0       . "#839496")
    (base1       . "#93a1a1")
    (base2       . "#eeedea")
    (base3       . "#fdfcf9")
    (yellow      . "#b58900")
    (orange      . "#cb4b16")
    (red         . "#dc322f")
    (magenta     . "#d33682")
    (violet      . "#6c71c4")
    (blue        . "#268bd2")
    (cyan        . "#2aa198")
    (green       . "#859900")
    (yellow-1bg  . "#f8e8c6")
    (yellow-1fg  . "#876d26")
    (yellow-2bg  . "#f1d49b")
    (yellow-2fg  . "#766634")
    (yellow-d    . "#866300")
    (yellow-l    . "#e1af4b")
    (orange-1bg  . "#fedfc5")
    (orange-1fg  . "#974727")
    (orange-2bg  . "#ffbd99")
    (orange-2fg  . "#854a33")
    (orange-d    . "#992700")
    (orange-l    . "#fb7640")
    (red-1bg     . "#ffdec8")
    (red-1fg     . "#a33c35")
    (red-2bg     . "#ffb9a1")
    (red-2fg     . "#8e433d")
    (red-d       . "#a7020a")
    (red-l       . "#ff6849")
    (magenta-1bg . "#fdded7")
    (magenta-1fg . "#9a3f6c")
    (magenta-2bg . "#fdbac6")
    (magenta-2fg . "#854568")
    (magenta-d   . "#a00559")
    (magenta-l   . "#ff699e")
    (violet-1bg  . "#ebe4e2")
    (violet-1fg  . "#4f5e99")
    (violet-2bg  . "#d1c9e3")
    (violet-2fg  . "#475a8b")
    (violet-d    . "#243e9b")
    (violet-l    . "#8d85e7")
    (blue-1bg    . "#e7e8e4")
    (blue-1fg    . "#1e6fa2")
    (blue-2bg    . "#c3d5e9")
    (blue-2fg    . "#246792")
    (blue-d      . "#0061a8")
    (blue-l      . "#74adf5")
    (cyan-1bg    . "#e4ecda")
    (cyan-1fg    . "#207e7b")
    (cyan-2bg    . "#bedfcf")
    (cyan-2fg    . "#247374")
    (cyan-d      . "#007d76")
    (cyan-l      . "#6ccec0")
    (green-1bg   . "#efeac7")
    (green-1fg   . "#657827")
    (green-2bg   . "#dbdb9c")
    (green-2fg   . "#5b6e35")
    (green-d     . "#5b7300")
    (green-l     . "#b3c34d")
    ;; palette end
    )
  "The solarized color palette alist.")

(solarized-create-theme-file 'light 'solarized-light-malb
  solarized-light-malb-color-palette-alist
  `((custom-theme-set-faces
     theme-name
     ;; steal org style from Leuven
     `(org-checkbox ((t (:weight bold :foreground ,base0
                         :box (:line-width 1 :color nil :style pressed-button)
                         :background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
     `(org-scheduled-previously ((t (:foreground ,orange))))
     `(org-tag ((t (:weight normal :box (:line-width 1 :color "#BBBBBB" :style nil) :foreground "#9A9FA4"))))
     `(org-todo ((t (:weight bold :box (:line-width 1 :color "#D8ABA7" :style nil) :foreground ,orange :background "#FFE6E4"))))
     `(org-done     ((t (:weight bold :box (:line-width 1 :color "#BBBBBB" :style nil) :foreground "#8BB300" :background "#F0F0F0"))))
     `(org-block-begin-line ((t (:slant normal :foreground ,s-base00 :background ,s-base2 :inherit (org-meta-line) :extend t))))
     `(org-block-end-line   ((t (:slant normal :foreground ,s-base00 :background ,s-base2 :inherit (org-meta-line) :extend t))))
     `(org-block ((t (:foreground ,s-base00 :background ,(solarized-color-blend "#cccccc" s-base3 0.2) :extend t))))

     `(org-agenda-structure
       ((,class (:foreground ,base1 :background ,base02
                 :weight bold :slant normal :inverse-video nil :height ,solarized-height-plus-1
                 :underline nil :extend t
                 :box (:line-width 2 :color ,base03)))))

     `(markdown-code-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2) :inherit nil :extend t))))
     `(markdown-language-keyword-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)
                                           :inherit (highlight)))))

     `(hl-sentence ((t (:background ,yellow-1bg :inherit (highlight)))))
     `(hl-line ((t (:background ,yellow-1bg :inherit (highlight)))))

     `(mu4e-header-highlight-face ((t (:background ,yellow-1bg :inherit (highlight)))))
     `(mu4e-related-face ((t (:foreground ,base00))))
     `(mu4e-thread-folding-child-face         ((t (:background "#EEF3FC" :extend t))))
     `(mu4e-thread-folding-root-unfolded-face ((t (:background "#DCE6F9" :extend t))))

     `(stripe-highlight ((t (:background ,s-base2 :extend t))))

     `(show-paren-match     ((,class (:foreground unspecified :background ,cyan-1bg :inherit (highlight) :extend t))))
     `(show-paren-mismatch  ((,class (:foreground ,base02 :background ,red :weight ,s-maybe-bold))))

     ;; steal spacemacs flycheck style
     `(flycheck-error   ((t (:underline (:color ,red :style line)))))
     `(flycheck-warning ((t (:underline (:color ,yellow :style line)))))
     `(flycheck-info    ((t (:underline (:color ,blue :style line)))))
     `(flycheck-fringe-error   ((t (:weight bold :foreground ,red :background ,s-base3))))
     `(flycheck-fringe-warning ((t (:weight bold :foreground ,yellow :background ,s-base3))))
     `(flycheck-fringe-info    ((t (:weight bold :foreground ,blue :background ,s-base3))))

     `(eshell-git-prompt-powerline-dir-face ((t (:foreground ,s-base3 :background ,blue))))
     `(eshell-git-prompt-powerline-clean-face ((t (:foreground ,s-base3 :background ,green))))
     `(eshell-git-prompt-powerline-not-clean-face ((t (:foreground ,s-base3 :background ,orange))))

     `(jupyter-repl-input-prompt ((t (:foreground ,green :weight bold))))
     `(jupyter-repl-output-prompt ((t (:foreground ,red :weight bold))))
     `(jupyter-repl-traceback ((t (:background ,yellow-1bg))))

     `(git-annex-dired-annexed-available ((t (:foreground ,green :weight bold))))
     `(git-annex-dired-annexed-unavailable ((t (:foreground ,red :weight bold))))

     `(dired-subtree-depth-1-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
     `(dired-subtree-depth-2-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
     `(dired-subtree-depth-3-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
     `(dired-subtree-depth-4-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
     `(dired-subtree-depth-5-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
     `(dired-subtree-depth-6-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))

     `(ein:cell-input-area   ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.15)))))
     `(ein:cell-input-prompt ((t (:weight bold :foreground ,s-base00 :inherit (header-line)))))

     `(lsp-headerline-breadcrumb-symbols-face ((t (:underline unspecified))))
     `(lsp-headerline-breadcrumb-symbols-error-face   ((t (:underline (:color ,red :style line)))))
     `(lsp-headerline-breadcrumb-symbols-hint-face    ((t (:underline (:color ,blue :style line)))))
     `(lsp-headerline-breadcrumb-symbols-info-face    ((t (:underline (:color ,blue :style line)))))
     `(lsp-headerline-breadcrumb-symbols-warning-face ((t (:underline (:color ,yellow :style line)))))

     `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,s-base00))))
     `(lsp-headerline-breadcrumb-path-error-face ((t (:underline unspecified))))
     `(lsp-headerline-breadcrumb-path-warning-face ((t (:underline unspecified))))
     `(lsp-headerline-breadcrumb-path-info-face ((t (:underline unspecified))))
     `(lsp-headerline-breadcrumb-path-hint-face ((t (:underline unspecified))))

     `(lsp-headerline-breadcrumb-project-prefix-face ((t (:underline unspecified))))
     `(lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:underline unspecified))))

     `(helm-ff-file-extension ((t ())))
     `(bm-face ((t (:background "#FFE6E4"))))

     `(mode-line
       ((,class (:inverse-video unspecified
                 :overline ,s-mode-line-bg
                 :underline ,s-mode-line-underline
                 :foreground ,s-mode-line-fg
                 :background ,(if solarized-high-contrast-mode-line
                                  s-mode-line-bg
                                yellow-1bg)
                 :box (:line-width 1 :color ,s-mode-line-bg
                       :style nil)))))
     `(mode-line-inactive
       ((,class (:inverse-video unspecified
                 :overline ,s-mode-line-inactive-bc
                 :underline ,s-mode-line-underline
                 :foreground ,s-mode-line-inactive-fg
                 :background ,s-mode-line-bg
                 :box (:line-width 1 :color ,s-mode-line-inactive-bg
                       :style nil)))))
     )

    (custom-theme-set-variables
     theme-name

     `(company-quickhelp-color-background ,s-base2)
     `(company-quickhelp-color-foreground ,s-base00)
     `(pdf-view-midnight-colors '(,s-base03 . ,s-base3))
     ;; `(pos-tip-background-color ,s-base2)
     ;; `(pos-tip-foreground-color ,s-base01)
     `(org-todo-keyword-faces
       '(("CANCELLED" :foreground "gray" :weight bold)
         ("ONHOLD"    :foreground "gray" :weight bold)
         ("STALLED"   :foreground "gray" :weight bold)
         ("SUBMITTED" :foreground "gray" :weight bold)
         ("DELEGATED" :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFF0E4")
         ("WAIT"      :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFF0E4")
         ("COAUTHOR"  :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFF0E4"))))
    t))
