;;; helm-baloo --- Helm interface to KDE's Baloo
;;; Commentary:

;;; Code:

(require 'helm-source)
(require 'helm-locate)

(defcustom helm-baloo-file-limit 100
  "Limit number of entries returned by baloo to this number."
  :group 'helm-baloo
  :type '(integer :tag "Limit"))

(defun baloo-search (pattern &optional directory)
  (if directory
      (start-process "baloosearch" nil "baloosearch" "-d" (expand-file-name directory)
                     "-l" (format "%d" helm-baloo-file-limit) pattern)
    (start-process "baloosearch" nil "baloosearch" "-l" (format "%d" helm-baloo-file-limit) pattern)))

(defun helm-baloo-search (&optional directory)
  (baloo-search helm-pattern directory))

(defun helm-baloo-transform (cs)
  (let '(helm-baloo-clean-up-regexp (rx (or
                                         control
                                         (seq "[0;31m" (+ (not (any "["))) "[0;0m")
                                         "[0;32m"
                                         "[0;0m")))
    (mapcar (function
             (lambda (c)
               (replace-regexp-in-string
                (rx (seq bol (+ space))) ""
                (replace-regexp-in-string helm-baloo-clean-up-regexp "" c))))
            cs)))

(defvar helm-baloo-actions
  '(("Open"                                  . helm-find-many-files)
    ("Attach to E-mail"                      . malb/helm-mml-attach-files)
    ("Transfer.sh"                           . malb/helm-transfer-sh-files)
    ("Find file as root"                     . helm-find-file-as-root)
    ("Find file other window"                . helm-find-files-other-window)
    ("Find file other frame"                 . find-file-other-frame)
    ("Open Dired in file's directory"        . helm-open-dired)
    ("Grep File(s) `C-u recurse'"            . helm-find-files-grep)
    ("Zgrep File(s) `C-u Recurse'"           . helm-ff-zgrep)
    ("Pdfgrep File(s)"                       . helm-ff-pdfgrep)
    ("Insert as org link"                    . helm-files-insert-as-org-link)
    ("Checksum File"                         . helm-ff-checksum)
    ("Ediff File"                            . helm-find-files-ediff-files)
    ("Ediff Merge File"                      . helm-find-files-ediff-merge-files)
    ("Etags `M-., C-u reload tag file'"      . helm-ff-etags-select)
    ("View file"                             . view-file)
    ("Insert file"                           . insert-file)
    ("Add marked files to file-cache"        . helm-ff-cache-add-file)
    ("Delete file(s)"                        . helm-delete-marked-files)
    ("Copy file(s) `M-C, C-u to follow'"     . helm-find-files-copy)
    ("Rename file(s) `M-R, C-u to follow'"   . helm-find-files-rename)
    ("Symlink files(s) `M-S, C-u to follow'" . helm-find-files-symlink)
    ("Relsymlink file(s) `C-u to follow'"    . helm-find-files-relsymlink)
    ("Hardlink file(s) `M-H, C-u to follow'" . helm-find-files-hardlink)
    ("Open file externally (C-u to choose)"  . helm-open-file-externally)
    ("Open file with default tool"           . helm-open-file-with-default-tool)
    ("Find file in hex dump"                 . hexl-find-file)))

(defun helm-baloo-no-directory ()
  (interactive)
  (helm :sources (helm-build-async-source "Baloo"
                   :candidates-process #'helm-baloo-search
                   :candidate-transformer '(helm-baloo-transform helm-skip-boring-files)
                   :action helm-baloo-actions
                   :keymap helm-generic-files-map
                   :help-message #'helm-generic-file-help-message)
        :buffer "*helm baloo*"))

(defun helm-baloo-in-directory (directory)
  (interactive "D")
  (helm :sources (helm-build-async-source "Baloo"
                   :candidates-process (lambda () (helm-baloo-search directory))
                   :candidate-transformer '(helm-baloo-transform helm-skip-boring-files)
                   :action helm-baloo-actions
                   :keymap helm-generic-files-map
                   :help-message #'helm-generic-file-help-message)
        :buffer "*helm baloo*"))

;;;###autoload
(defun helm-baloo (&optional arg)
  (interactive "P")
  (if arg
      (progn
        (call-interactively #'helm-baloo-in-directory))
    (call-interactively #'helm-baloo-no-directory)))

(provide 'helm-baloo)
;;; helm-baloo.el ends here
