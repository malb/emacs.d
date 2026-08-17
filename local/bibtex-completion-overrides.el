;;; bibtex-completion-overrides --- my local tweaks to bibtex-completion
;;; Commentary:

;;; Code:

(require 'bibtex-completion)

;;;;;;;;;;;;;;;;;;;;
;; PDF Filenames
;;;;;;;;;;;;;;;;;;;;

;; Indexing the file names of `bibtex-completion-library-path' once
;; turns the per-entry PDF check from one file test per directory
;; (which dominates the parse time for large bibliographies) into a
;; hash lookup.

(defvar malb/bibtex-completion-pdf-ht nil
  "Hash table mapping PDF file names to t, for all library directories.")

(defvar malb/bibtex-completion-pdf-ht-mtimes nil
  "Alist (DIR . MTIME) as recorded when `malb/bibtex-completion-pdf-ht' was built.")

(defvar malb/bibtex-completion-pdf-ht-frozen nil
  "Non-nil while reparsing the bibliography; suppresses freshness checks.")

(defun malb/bibtex-completion-pdf-ht-rebuild ()
  "Rebuild `malb/bibtex-completion-pdf-ht' from the library directories."
  (let ((ht (make-hash-table :test 'equal :size 4096))
        (mtimes nil))
    (dolist (dir (-flatten (list bibtex-completion-library-path)))
      (dolist (file (ignore-errors (directory-files dir t)))
        (puthash (file-name-nondirectory file) t ht))
      (push (cons dir (ignore-errors (file-attribute-mtime dir))) mtimes))
    (setq malb/bibtex-completion-pdf-ht ht
          malb/bibtex-completion-pdf-ht-mtimes mtimes)))

(defun malb/bibtex-completion-pdf-ht ()
  "Return `malb/bibtex-completion-pdf-ht', rebuilding it if stale."
  (unless (or malb/bibtex-completion-pdf-ht-frozen
              (and malb/bibtex-completion-pdf-ht
                   (cl-loop for (dir . mtime) in malb/bibtex-completion-pdf-ht-mtimes
                            always (equal mtime (ignore-errors (file-attribute-mtime dir))))))
    (malb/bibtex-completion-pdf-ht-rebuild))
  malb/bibtex-completion-pdf-ht)

(defun malb/bibtex-completion-find-pdf-in-library (key-or-entry &optional find-additional)
  "Search the directories in `bibtex-completion-library-path' by KEY-OR-ENTRY.

The path of the first matching PDF is returned.

If FIND-ADDITIONAL is non-nil, the paths of all PDFs whose name
starts with the BibTeX key and ends with
`bibtex-completion-pdf-extension' are returned instead."
  (let* ((key (if (stringp key-or-entry)
                  key-or-entry
                (bibtex-completion-get-value "=key=" key-or-entry)))
         (key (s-replace ":" "_" key))
         (main-pdf (cl-loop
                    for ext in (-flatten (list bibtex-completion-pdf-extension))
                    for name = (concat key ext)
                    when (gethash name (malb/bibtex-completion-pdf-ht))
                    append (mapcar (lambda (dir)
                                     (f-join dir name))
                                   (-flatten (list bibtex-completion-library-path))))))
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
  (when (not (equal (malb/bibtex-completion-hashes) malb/bibtex-completion-hashes))
    ;; Rebuild the PDF name index right before the reparse, and keep
    ;; it frozen for the duration of the reparse so that the
    ;; per-entry PDF checks are pure hash lookups:
    (malb/bibtex-completion-pdf-ht-rebuild)
    (setq malb/bibtex-completion-candidates-cache
          (let ((malb/bibtex-completion-pdf-ht-frozen t))
            (apply old-function arguments)))
    ;; Recompute the stored identity *after* reparsing, so that it
    ;; reflects the freshly parsed cache and the next call hits the
    ;; memo instead of re-reading and re-hashing all files.
    (setq malb/bibtex-completion-hashes (malb/bibtex-completion-hashes)))
  malb/bibtex-completion-candidates-cache)

(advice-add #'bibtex-completion-candidates :around #'malb/bibtex-completion-candidates-cache)

(provide 'bibtex-completion-overrides)
;;; bibtex-completion-overrides.el ends here
