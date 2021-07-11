;;; bibtex-completion-overrides --- my local tweaks to bibtex-completion
;;; Commentary:

;;; Code:

(require 'bibtex-completion)

;;;;;;;;;;;;;;;;;;;;
;; PDF Filenames
;;;;;;;;;;;;;;;;;;;;


(defun malb/bibtex-completion-find-pdf-in-library (key-or-entry &optional find-additional)
  "Search the directories in `bibtex-completion-library-path' by KEY-OR-ENTRY.

The path of the first matching PDF is returned.

If FIND-ADDITIONAL is non-nil, the paths of all PDFs whose name
starts with the BibTeX key and ends with
`bibtex-completion-pdf-extension' are returned instead."
  (let* ((key (if (stringp key-or-entry)
                  key-or-entry
                (bibtex-completion-get-value "=key=" key-or-entry)))
         (main-pdf (cl-loop
                    for dir in
                    (-flatten bibtex-completion-library-path)
                    append (cl-loop
                            for ext in
                            (-flatten bibtex-completion-pdf-extension)
                            collect (f-join dir (s-concat
                                                 (s-replace ":" "_" key) ext)))))) ;; the tweak
    (if find-additional
        (sort                           ; move main pdf on top of the list if needed
         (cl-loop
          for dir in (-flatten bibtex-completion-library-path)
          append (directory-files dir t
                                  (s-concat "^" (regexp-quote (s-replace ":" "_" key))
                                            ".*\\("
                                            (mapconcat
                                             'regexp-quote
                                             (-flatten bibtex-completion-pdf-extension)
                                             "\\|")
                                            "\\)$")))
         (lambda (x y)
           (and (member x main-pdf)
                (not (member y main-pdf)))))
      (-flatten (-first 'f-file? main-pdf)))))

(defun malb/bibtex-completion-add-pdf-to-library (keys)
  "Add a PDF to the library for the first selected entry.

The PDF can be added either from an open buffer or a file."
  (let* ((key (car keys))
         (source (char-to-string
                  (read-char-choice "Add pdf from [b]uffer or [f]ile? " '(?b ?f))))
         (buffer (when (string= source "b")
                   (read-buffer-to-switch "Add pdf buffer: ")))
         (file (when (string= source "f")
                 (expand-file-name (read-file-name "Add pdf file: " nil nil t))))
         (path (-flatten (list bibtex-completion-library-path)))
         (path (if (cdr path)
                   (completing-read "Add pdf to: " path nil t)
                 (car path)))
         (pdf (expand-file-name (concat  (s-replace ":" "_" key) ".pdf") path)))
    (cond
     (buffer
      (with-current-buffer buffer
        (write-file pdf)))
     (file
      (copy-file file pdf)))))

(advice-add #'bibtex-completion-find-pdf-in-library :override #'malb/bibtex-completion-find-pdf-in-library)
(advice-add #'bibtex-completion-add-pdf-to-library  :override #'malb/bibtex-completion-add-pdf-to-library)

;;
;; Caching
;;

(defvar malb/bibtex-completion-hashes nil)

(defun malb/bibtex-completion-hashes ()
  "Return all the hashes."
  (cons bibtex-completion-bibliography
        (mapcar 'cadr bibtex-completion-cache)))

(defvar malb/bibtex-completion-candidates-cache nil)

(defun malb/bibtex-completion-candidates-clear-cache ()
  "Clear the cache."
  (setq malb/bibtex-completion-candidates-cache nil
        malb/bibtex-completion-hashes nil))

(defun malb/bibtex-completion-candidates-cache (old-function &rest arguments)
  "Call `bibtex-completion-candidates` only when files changed."

  (let ((local-hashes (malb/bibtex-completion-hashes)))
    (when (not (equal local-hashes malb/bibtex-completion-hashes))
      (setq malb/bibtex-completion-hashes local-hashes
            malb/bibtex-completion-candidates-cache (apply old-function arguments))))
  malb/bibtex-completion-candidates-cache)

(advice-add #'bibtex-completion-candidates :around #'malb/bibtex-completion-candidates-cache)

(provide 'bibtex-completion-overrides)
;;; bibtex-completion-overrides.el ends here
