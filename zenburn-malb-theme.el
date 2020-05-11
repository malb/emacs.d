(deftheme zenburn-malb
  "my modifications on top of zenburn")

(custom-theme-set-faces
 'zenburn-malb
 '(spaceline-python-venv ((t (:foreground "#69B7F0")))))

(custom-theme-set-variables
 'zenburn-malb
 '(pdf-view-midnight-colors '("#DCDCCC" . "#3F3F3F"))
 '(org-todo-keyword-faces
   '(("CANCELLED" :foreground "gray" :weight bold)
     ("ONHOLD"    :foreground "gray" :weight bold)
     ("STALLED"   :foreground "gray" :weight bold)
     ("SUBMITTED" :foreground "gray" :weight bold)
     ("DELEGATED" :foreground "dark orange" :weight bold)
     ("WAIT"   :foreground "dark orange" :weight bold)
     ("COAUTHOR"  :foreground "dark orange" :weight bold))))

(provide-theme 'zenburn-malb)
