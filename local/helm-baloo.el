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
      (start-process "baloosearch" nil "baloosearch" "-d" directory "-l" (format "%d" helm-baloo-file-limit) pattern)
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
  '(("Open"                   . (lambda (x) (helm-find-many-files x)))
    ("Attach to E-mail"        . (lambda (x) (malb/helm-mml-attach-files x)))
    ("Transfer.sh"            . (lambda (x) (malb/helm-transfer-sh-files x)))
    ("Find file as root"      . (lambda (x) (helm-find-file-as-root x)))
    ("Find file other window" . (lambda (x) (helm-find-files-other-window x)))
    ("Find file other frame"  . (lambda (x) (find-file-other-frame x)))
    ("Open Dired in file's directory" . (lambda (x) (helm-open-dired x)))
    ("Grep File(s) `C-u recurse'"     . (lambda (x) (helm-find-files-grep x)))
    ("Zgrep File(s) `C-u Recurse'"    . (lambda (x) (helm-ff-zgrep x)))
    ("Pdfgrep File(s)"                . (lambda (x) (helm-ff-pdfgrep x)))
    ("Insert as org link"             . (lambda (x) (helm-files-insert-as-org-link x)))
    ("Checksum File"                  . (lambda (x) (helm-ff-checksum x)))
    ("Ediff File"                     . (lambda (x) (helm-find-files-ediff-files x)))
    ("Ediff Merge File" . (lambda (x) (helm-find-files-ediff-merge-files x)))
    ("Etags `M-., C-u reload tag file'" . (lambda (x) (helm-ff-etags-select x)))
    ("View file" . (lambda (x) (view-file x)))
    ("Insert file" . (lambda (x) (insert-file x)))
    ("Add marked files to file-cache" . (lambda (x) (helm-ff-cache-add-file x)))
    ("Delete file(s)" . (lambda (x) (helm-delete-marked-files x)))
    ("Copy file(s) `M-C, C-u to follow'" . (lambda (x) (helm-find-files-copy x)))
    ("Rename file(s) `M-R, C-u to follow'"  . (lambda (x) (helm-find-files-rename x)))
    ("Symlink files(s) `M-S, C-u to follow'" . (lambda (x) (helm-find-files-symlink x)))
    ("Relsymlink file(s) `C-u to follow'"   . (lambda (x) (helm-find-files-relsymlink x)))
    ("Hardlink file(s) `M-H, C-u to follow'" . (lambda (x) (helm-find-files-hardlink x)))
    ("Open file externally (C-u to choose)"  . (lambda (x) (helm-open-file-externally x)))
    ("Open file with default tool" . (lambda (x) (helm-open-file-with-default-tool x)))
    ("Find file in hex dump" . (lambda (x) (hexl-find-file) x))))

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
