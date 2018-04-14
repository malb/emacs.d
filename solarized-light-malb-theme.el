(deftheme solarized-light-malb
  "my modifications on top of solarized-light")

(custom-theme-set-faces
 'solarized-light-malb
 ;; steal org style from Leuven
 '(org-checkbox ((t (:weight bold :box (:line-width 1 :color nil :style (quote pressed-button)) :foreground "white" :background "light gray"))))
 '(org-done ((t (:weight bold :box (:line-width 1 :color "#BBBBBB" :style nil) :foreground "#8BB300" :background "#F0F0F0"))))
 '(org-scheduled-previously ((t (:foreground "#cb4b16"))))
 '(org-tag ((t (:weight normal :box (:line-width 1 :color "#BBBBBB" :style nil) :foreground "#9A9FA4"))))
 '(org-todo ((t (:weight bold :box (:line-width 1 :color "#D8ABA7" :style nil) :foreground "#cb4b16" :background "#FFE6E4"))))
 '(org-block-begin-line ((t (:slant normal :foreground "#657b83" :background "#eee8d5" :inherit (org-meta-line)))))
 '(org-block-end-line ((t (:slant normal :foreground "#657b83" :background "#eee8d5" :inherit (org-meta-line)))))
 '(org-block ((t (:foreground "#657b83" :background "#f8f1de"))))

 '(auto-dim-other-buffers-face ((t (:background "#f1ead7"))))
 '(hl-sentence ((t (:background "#f3edda" :inherit (highlight)))))
 '(which-func ((t (:foreground "#DEB542"))))

 ;; steal spacemacs flycheck style
 '(flycheck-error ((t (:underline (:color "#dc322f" :style line)))))
 '(flycheck-warning ((t (:underline (:color "#b58900" :style line)))))
 '(flycheck-info ((t (:underline (:color "#268bd2" :style line)))))
 '(flycheck-fringe-warning ((t (:weight bold :foreground "#DEB542" :background "#fdf6e3"))))
 '(flycheck-fringe-error ((t (:weight bold :foreground "#dc322f" :background "#fdf6e3"))))
 '(flycheck-fringe-info ((t (:weight bold :foreground "#69B7F0" :background "#fdf6e3"))))

 '(eshell-git-prompt-powerline-dir-face ((t (:foreground "#fdf6e3" :background "steel blue"))))
 '(eshell-git-prompt-powerline-clean-face ((t (:foreground "#fdf6e3" :background "forest green"))))
 '(eshell-git-prompt-powerline-not-clean-face ((t (:foreground "#fdf6e3" :background "indian red"))))

 '(dired-subtree-depth-1-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-2-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-3-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-4-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-5-face ((t (:background "#f8f1de"))))
 '(dired-subtree-depth-6-face ((t (:background "#f8f1de"))))

 '(rtags-fixitline ((t (:slant normal :underline nil))))
 '(rtags-errline ((t (:slant normal :underline nil))))
 '(rtags-warnline ((t (:slant normal :underline nil))))
 '(rtags-skippedline ((t (:background "#eee8d5"))))

 '(ein:cell-input-area ((t (:background "#f5efdc"))))
 '(ein:cell-input-prompt ((t (:weight bold :foreground "#657b83" :inherit (header-line))))))

(provide-theme 'solarized-light-malb)
