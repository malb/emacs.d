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
  `((custom-theme-set-faces theme-name
                            ;; steal org style from Leuven
                            `(org-checkbox ((t (:weight bold :box (:line-width 1 :color nil :style (quote pressed-button))
                                                        :foreground "white" :background "light gray"))))
                            `(org-done     ((t (:weight bold :box (:line-width 1 :color "#BBBBBB" :style nil) :foreground "#8BB300" :background "#F0F0F0"))))
                            `(org-scheduled-previously ((t (:foreground ,orange))))
                            `(org-tag ((t (:weight normal :box (:line-width 1 :color "#BBBBBB" :style nil) :foreground "#9A9FA4"))))
                            `(org-todo ((t (:weight bold :box (:line-width 1 :color "#D8ABA7" :style nil) :foreground ,orange :background "#FFE6E4"))))
                            `(org-block-begin-line ((t (:slant normal :foreground ,s-base00 :background ,s-base2 :inherit (org-meta-line)))))
                            `(org-block-end-line   ((t (:slant normal :foreground ,s-base00 :background ,s-base2 :inherit (org-meta-line)))))
                            `(org-block ((t (:foreground ,s-base00 :background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))

                            `(markdown-code-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2) :inherit nil ))))
                            `(markdown-language-keyword-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2) :inherit (highlight)))))

                            `(hl-sentence ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2) :inherit (highlight)))))
                            `(which-func  ((t (:foreground ,green :weight bold))))
                            `(stripe-highlight ((t (:background ,s-base2))))

                            `(sp-wrap-overlay-opening-pair ((t (:foreground ,green))))
                            `(sp-wrap-overlay-closing-pair ((t (:foreground ,orange))))

                            ;; steal spacemacs flycheck style
                            `(flycheck-error   ((t (:underline (:color "#dc322f" :style line)))))
                            `(flycheck-warning ((t (:underline (:color "#b58900" :style line)))))
                            `(flycheck-info    ((t (:underline (:color "#268bd2" :style line)))))
                            `(flycheck-fringe-warning ((t (:weight bold :foreground "#DEB542" :background ,s-base3))))
                            `(flycheck-fringe-error   ((t (:weight bold :foreground "#dc322f" :background ,s-base3))))
                            `(flycheck-fringe-info    ((t (:weight bold :foreground "#69B7F0" :background ,s-base3))))

                            `(eshell-git-prompt-powerline-dir-face ((t (:foreground ,s-base3 :background ,blue))))
                            `(eshell-git-prompt-powerline-clean-face ((t (:foreground ,s-base3 :background ,green))))
                            `(eshell-git-prompt-powerline-not-clean-face ((t (:foreground ,s-base3 :background ,orange))))

                            `(dired-subtree-depth-1-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
                            `(dired-subtree-depth-2-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
                            `(dired-subtree-depth-3-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
                            `(dired-subtree-depth-4-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
                            `(dired-subtree-depth-5-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))
                            `(dired-subtree-depth-6-face ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.2)))))

                            `(ein:cell-input-area   ((t (:background ,(solarized-color-blend "#cccccc" s-base3 0.15)))))
                            `(ein:cell-input-prompt ((t (:weight bold :foreground ,s-base00 :inherit (header-line)))))

                            `(bm-face ((t (:background "#FFE6E4"))))
                            `(helm-locate-finish ((t (:foreground ,green :weight bold))))
                            `(slack-message-output-header ((t (:weight bold))))
                            `(slack-message-output-text ((t (:weight normal))))
                            `(slack-new-message-marker-face ((t (:foreground "#d33682" :weight bold :height 0.75))))
                            `(helm-xref-file-name ((t (:foreground ,s-base0)))))

    (custom-theme-set-variables theme-name
                                `(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
                                `(highlight-symbol-colors
                                  (--map
                                   (solarized-color-blend
                                    it
                                    ,s-base3
                                    0.25)
                                   (quote
                                    (,yellow
                                     ,cyan
                                     ,red
                                     ,violet
                                     ,green
                                     ,orange
                                     ,blue))))
                                `(highlight-symbol-foreground-color "#586e75")
                                `(highlight-tail-colors
                                  (quote
                                   ((,s-base2 . 0)
                                    ("#B4C342" . 20)
                                    ("#69CABF" . 30)
                                    ("#69B7F0" . 50)
                                    ("#DEB542" . 60)
                                    ("#F2804F" . 70)
                                    ("#F771AC" . 85)
                                    (,s-base2 . 100))))
                                `(hl-bg-colors (quote ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
                                `(hl-fg-colors (quote (,s-base3 ,s-base3 ,s-base3 ,s-base3 ,s-base3 ,s-base3 ,s-base3 ,s-base3)))
                                `(hl-paren-colors
                                  (quote
                                   ("#2aa198"
                                    "#b58900"
                                    "#268bd2"
                                    "#6c71c4"
                                    "#859900")))
                                `(nrepl-message-colors (quote ("#dc322f" ,orange "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
                                `(smartrep-mode-line-active-bg (solarized-color-blend "#859900" ,s-base2 0.2))
                                `(vc-annotate-background nil)
                                `(vc-annotate-background-mode nil)

                                `(company-quickhelp-color-background ,s-base2)
                                `(company-quickhelp-color-foreground ,s-base00)
                                `(pdf-view-midnight-colors '(,s-base03 . ,s-base3))
                                `(pos-tip-background-color ,s-base2)
                                `(pos-tip-foreground-color ,s-base01)
                                `(org-todo-keyword-faces
                                  '(("CANCELLED" :foreground "gray" :weight bold)
                                    ("DISABLED"  :foreground "gray" :weight bold)
                                    ("ONHOLD"    :foreground "gray" :weight bold)
                                    ("STALLED"   :foreground "gray" :weight bold)
                                    ("SUBMITTED" :foreground "gray" :weight bold)
                                    ("DELEGATED" :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFE6E4")
                                    ("WAITING"   :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFE6E4")
                                    ("COAUTHOR"  :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFE6E4"))))
    t))
