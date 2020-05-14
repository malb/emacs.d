(require 'solarized-palettes)

(solarized-create-theme-file 'dark 'solarized-zenburn-malb
  solarized-zenburn-color-palette-alist
  `((custom-theme-set-faces
     theme-name
     )

    (custom-theme-set-variables theme-name
                                `(company-quickhelp-color-background ,s-base2)
                                `(company-quickhelp-color-foreground ,s-base00)
                                `(pdf-view-midnight-colors '(,s-base03 . ,s-base3))
                                `(org-todo-keyword-faces
                                  '(("CANCELLED" :foreground "gray" :weight bold)
                                    ("ONHOLD"    :foreground "gray" :weight bold)
                                    ("STALLED"   :foreground "gray" :weight bold)
                                    ("SUBMITTED" :foreground "gray" :weight bold)
                                    ("DELEGATED" :foreground "dark orange" :weight bold)
                                    ("WAIT"      :foreground "dark orange" :weight bold)
                                    ("COAUTHOR"  :foreground "dark orange" :weight bold))))
    t))
