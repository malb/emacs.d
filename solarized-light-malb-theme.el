(deftheme solarized-light-malb
  "my modifications on top of solarized-light")

(custom-theme-set-faces
 'solarized-light-malb
 ;; steal org style from Leuven
 '(org-checkbox ((t (:weight bold :box (:line-width 1 :color nil :style (quote pressed-button))
                             :foreground "white" :background "light gray"))))
 '(org-done     ((t (:weight bold :box (:line-width 1 :color "#BBBBBB" :style nil) :foreground "#8BB300" :background "#F0F0F0"))))
 '(org-scheduled-previously ((t (:foreground "#cb4b16"))))
 '(org-tag ((t (:weight normal :box (:line-width 1 :color "#BBBBBB" :style nil) :foreground "#9A9FA4"))))
 '(org-todo ((t (:weight bold :box (:line-width 1 :color "#D8ABA7" :style nil) :foreground "#cb4b16" :background "#FFE6E4"))))
 '(org-block-begin-line ((t (:slant normal :foreground "#657b83" :background "#eee8d5" :inherit (org-meta-line)))))
 '(org-block-end-line   ((t (:slant normal :foreground "#657b83" :background "#eee8d5" :inherit (org-meta-line)))))
 '(org-block ((t (:foreground "#657b83" :background "#f8f1de"))))

 '(auto-dim-other-buffers-face ((t (:background "#f1ead7"))))
 '(hl-sentence ((t (:background "#f3edda" :inherit (highlight)))))
 '(which-func  ((t (:foreground "#DEB542"))))
 '(stripe-highlight ((t (:background "#eee8d5"))))

 '(sp-wrap-overlay-opening-pair ((t (:foreground "#859900"))))
 '(sp-wrap-overlay-closing-pair ((t (:foreground "#cb4b16"))))

 ;; steal spacemacs flycheck style
 '(flycheck-error   ((t (:underline (:color "#dc322f" :style line)))))
 '(flycheck-warning ((t (:underline (:color "#b58900" :style line)))))
 '(flycheck-info    ((t (:underline (:color "#268bd2" :style line)))))
 '(flycheck-fringe-warning ((t (:weight bold :foreground "#DEB542" :background "#fdf6e3"))))
 '(flycheck-fringe-error   ((t (:weight bold :foreground "#dc322f" :background "#fdf6e3"))))
 '(flycheck-fringe-info    ((t (:weight bold :foreground "#69B7F0" :background "#fdf6e3"))))

 '(eshell-git-prompt-powerline-dir-face ((t (:foreground "#fdf6e3" :background "steel blue"))))
 '(eshell-git-prompt-powerline-clean-face ((t (:foreground "#fdf6e3" :background "forest green"))))
 '(eshell-git-prompt-powerline-not-clean-face ((t (:foreground "#fdf6e3" :background "indian red"))))

 '(dired-subtree-depth-1-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-2-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-3-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-4-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-5-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-6-face ((t (:background "#f8f1de"))))

 '(rtags-fixitline   ((t (:slant normal :underline nil))))
 '(rtags-errline     ((t (:slant normal :underline nil))))
 '(rtags-warnline    ((t (:slant normal :underline nil))))
 '(rtags-skippedline ((t (:background "#eee8d5"))))

 '(ein:cell-input-area   ((t (:background "#f5efdc"))))
 '(ein:cell-input-prompt ((t (:weight bold :foreground "#657b83" :inherit (header-line)))))

 '(powerline-active1    ((t (:background "grey22" :foreground "white smoke"))))
 '(powerline-active2    ((t (:background "grey40" :foreground "gainsboro"))))
 '(powerline-inactive1  ((t (:background "grey55" :foreground "white smoke"))))
 '(powerline-inactive2  ((t (:background "grey65" :foreground "gainsboro"))))
 '(mode-line-buffer-id  ((t (:foreground "gray40"))))

 '(minimap-active-region-background ((t (:background nil))))
 '(minimap-font-face ((t (:height 0.2))))

 '(bm-face ((t (:background "#FFE6E4"))))
 '(font-latex-slide-title-face ((t (:inherit (variable-pitch font-lock-type-face) :weight bold))))
 '(helm-locate-finish ((t (:foreground "forest green"))))
 '(slack-message-output-header ((t (:weight bold))))
 '(slack-message-output-text ((t (:weight normal))))
 '(slack-new-message-marker-face ((t (:foreground "#d33682" :weight bold :height 0.75))))
 )

(custom-theme-set-variables
 'solarized-light-malb
 '(ansi-color-names-vector ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend
     it
     "#fdf6e3"
     0.25)
    (quote
     ("#b58900"
      "#2aa198"
      "#dc322f"
      "#6c71c4"
      "#859900"
      "#cb4b16"
      "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors (quote ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors (quote ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors
   (quote
    ("#2aa198"
     "#b58900"
     "#268bd2"
     "#6c71c4"
     "#859900")))
 '(nrepl-message-colors (quote ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '(((20 . "#dc322f")
      (40 . "#c85d17")
      (60 . "#be730b")
      (80 . "#b58900")
      (100 . "#a58e00")
      (120 . "#9d9100")
      (140 . "#959300")
      (160 . "#8d9600")
      (180 . "#859900")
      (200 . "#669b32")
      (220 . "#579d4c")
      (240 . "#489e65")
      (260 . "#399f7e")
      (280 . "#2aa198")
      (300 . "#2898af")
      (320 . "#2793ba")
      (340 . "#268fc6")
      (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified
     "#fdf6e3"
     "#eee8d5"
     "#990A1B"
     "#dc322f"
     "#546E00"
     "#859900"
     "#7B6000"
     "#b58900"
     "#00629D"
     "#268bd2"
     "#93115C"
     "#d33682"
     "#00736F"
     "#2aa198"
     "#657b83"
     "#839496")))
 '(xterm-color-names        ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(company-quickhelp-color-background "#eee8d5")
 '(company-quickhelp-color-foreground "#657b83")
 '(pdf-view-midnight-colors '("#323d41" . "#fdf6e3"))
 '(beacon-color "#EEAD0E")
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(org-todo-keyword-faces
   '(("CANCELLED" :foreground "gray" :weight bold)
     ("DISABLED"  :foreground "gray" :weight bold)
     ("ONHOLD"    :foreground "gray" :weight bold)
     ("STALLED"   :foreground "gray" :weight bold)
     ("SUBMITTED" :foreground "gray" :weight bold)
     ("DELEGATED" :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFE6E4")
     ("WAITING"   :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFE6E4")
     ("COAUTHOR"  :foreground "dark orange" :weight bold :box (:line-width 1 :color "#D8ABA7") :background "#FFE6E4"))))

(provide-theme 'solarized-light-malb)
