(deftheme zenburn-malb
  "my modifications on top of zenburn")

(custom-theme-set-faces
 'zenburn-malb)

(custom-theme-set-variables
 'zenburn-malb
 '(pdf-view-midnight-colors '("#DCDCCC" . "#3F3F3F"))
 '(org-todo-keyword-faces
   '(("CANCELLED" :foreground "gray" :weight bold)
     ("DISABLED"  :foreground "gray" :weight bold)
     ("ONHOLD"    :foreground "gray" :weight bold)
     ("STALLED"   :foreground "gray" :weight bold)
     ("SUBMITTED" :foreground "gray" :weight bold)
     ("DELEGATED" :foreground "dark orange" :weight bold :box (:line-width 0))
     ("WAITING"   :foreground "dark orange" :weight bold :box (:line-width 0))
     ("COAUTHOR"  :foreground "dark orange" :weight bold :box (:line-width 0)))))

(provide-theme 'zenburn-malb)
