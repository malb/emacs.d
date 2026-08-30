;;; helm-baloo --- Helm interface to KDE's Baloo
;;; Commentary:

;;; Code:

(require 'helm-source)
(require 'helm-locate)
(require 'helm-files)

(defcustom helm-baloo-file-limit 100
  "Limit number of entries returned by baloo to this number."
  :group 'helm-baloo
  :type '(integer :tag "Limit"))

(defun baloo-search (pattern &optional directory)
  (if directory
      (start-process "baloosearch" nil "baloosearch6" "-d" (expand-file-name directory)
                     "-l" (format "%d" helm-baloo-file-limit) pattern)
    (start-process "baloosearch" nil "baloosearch6" "-l" (format "%d" helm-baloo-file-limit) pattern)))

(defun helm-baloo-search (&optional directory)
  (baloo-search helm-pattern directory))

(defun helm-baloo-transform (candidates)
  (cl-loop for i in candidates
           collect (ansi-color-apply i)))

(defvar helm-baloo-actions
  '(("Open"                                  . helm-find-many-files)
    ("Attach to E-mail"                      . malb/helm-mml-attach-files)
    ;; ("Transfer.sh"                           . malb/helm-transfer-sh-files)
    ("Find file as root"                     . helm-find-file-as-root)
    ("Find file other window"                . helm-find-files-other-window)
    ("Find file other frame"                 . find-file-other-frame)
    ("Open Dired in file's directory `C-d'"  . helm-open-dired)
    ("Insert as org link"                    . helm-files-insert-as-org-link)
    ("Checksum File"                         . helm-ff-checksum)
    ("Ediff File"                            . helm-find-files-ediff-files)
    ("View file"                             . view-file)
    ("Insert file"                           . insert-file)
    ("Open file externally (C-u to choose)"  . helm-open-file-externally)
    ("Open file with default tool"           . helm-open-file-with-default-tool)
    ("Copy file(s) `M-C, C-u' to follow"     . helm-find-files-copy)
    ("Rename file(s) `M-R, C-u' to follow"   . helm-find-files-rename)
    ("Symlink files(s) `M-S, C-u' to follow" . helm-find-files-symlink)))

(defvar helm-baloo-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-]")     'helm-ff-run-toggle-basename)
    (define-key map (kbd "M-R")     'helm-ff-run-rename-file)
    (define-key map (kbd "M-C")     'helm-ff-run-copy-file)
    (define-key map (kbd "M-B")     'helm-ff-run-byte-compile-file)
    (define-key map (kbd "M-L")     'helm-ff-run-load-file)
    (define-key map (kbd "M-S")     'helm-ff-run-symlink-file)
    (define-key map (kbd "M-H")     'helm-ff-run-hardlink-file)
    (define-key map (kbd "M-D")     'helm-ff-run-delete-file)
    (define-key map (kbd "C-=")     'helm-ff-run-ediff-file)
    (define-key map (kbd "C-c o")   'helm-ff-run-switch-other-window)
    (define-key map (kbd "C-c r")   'helm-ff-run-find-file-as-root)
    (define-key map (kbd "C-c C-o") 'helm-ff-run-switch-other-frame)
    (define-key map (kbd "M-i")     'helm-ff-properties-persistent)
    (define-key map (kbd "C-c C-x") 'helm-ff-run-open-file-externally)
    (define-key map (kbd "C-c X")   'helm-ff-run-open-file-with-default-tool)
    (define-key map (kbd "C-c @")   'helm-ff-run-insert-org-link)
    (define-key map (kbd "C-d")     (lambda () (interactive)
                                      (with-helm-alive-p
                                        (helm-exit-and-execute-action
                                         'helm-open-dired))))
    (define-key map (kbd "C-c C-a") 'malb/helm-mml-attach-files)
    map)
  "Keymap for Helm Baloo.")

(defun helm-baloo-no-directory ()
  "Run `baloosearch' without any directory restriction."
  (interactive)
  (helm :sources (helm-build-async-source "Baloo"
                   :candidates-process #'helm-baloo-search
                   :candidate-transformer '(helm-baloo-transform helm-skip-boring-files)
                   :action helm-baloo-actions
                   :keymap helm-baloo-map
                   :requires-pattern 3
                   :help-message #'helm-generic-file-help-message)
        :buffer "*helm baloo*"))

(defun helm-baloo-in-directory (directory)
  "Run `baloosearch' restricted to DIRECTORY."
  (interactive "D")
  (helm :sources (helm-build-async-source "Baloo"
                   :candidates-process (lambda () (helm-baloo-search directory))
                   :candidate-transformer '(helm-baloo-transform helm-skip-boring-files)
                   :action helm-baloo-actions
                   :keymap helm-baloo-map
                   :requires-pattern 3
                   :help-message #'helm-generic-file-help-message)
        :buffer "*helm baloo*"))

;;;###autoload
(defun helm-baloo (&optional arg)
  "Run `baloosearch'. If ARG is given, set a directory first"
  (interactive "P")
  (if arg
      (progn
        (call-interactively #'helm-baloo-in-directory))
    (call-interactively #'helm-baloo-no-directory)))

(provide 'helm-baloo)

;;; helm-baloo.el ends here
