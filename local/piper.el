;;; piper --- Speak text with Piper
;;;
;;; See https://github.com/rhasspy/piper
;;;
;;; Commentary:
;;; Code:

(require 'emms)
(require 'guess-language)
(require 'pandoc-mode-utils)
(require 'f)
(require 's)

(defgroup piper nil
  "Run Piper for Text to Speech."
  :group 'tools)

(defcustom piper-voices nil
  "Voices to use in Piper in order of preference.

These are tuples of (\"model-path\", \"lang-code\")."

  :group 'piper
  :type '(alist :value-type (string string)))

(defcustom piper-command "cat ${input-filename} | piper -m ${voice} -f ${output-filename}"
  "Command to run Piper."
  :group 'piper
  :type 'string)

(defun piper-is-quote (text)
  "Decide if TEXT is a quote."
  (equal (and (>= (length text) 2) (substring text 0 2)) "  "))

;; Return the first voice matching detected language.

(defun piper-select-voice (text)
  "Select voice by picking first voice matching detected language in TEXT."
  (let ((lang (with-temp-buffer
                (insert text)
                (replace-regexp-in-string "_" "-" (cadr (assq (guess-language-buffer) guess-language-langcodes)))))
        (voices nil))
    (dolist (entry piper-voices)
      (if (string-prefix-p lang (cadr entry))
          (push (car entry) voices)))
    (setq voices (nreverse voices))
    (cond
     ((eq (length voices) 0) (caar piper-voices))
     ((eq (length voices) 1) (car voices))
     (t (if (piper-is-quote text)
            (cadr voices)
          (car voices))))))

(defun piper-voices-completing-read ()
  "Offer list of installed Piper voices to choose from and return choice."
  (replace-regexp-in-string
   " .*$" "" (completing-read "Voice: "
                              (mapcar (lambda (x) (format "%s (%s)" (car x) (cadr x)))
                                      piper-voices)
                              nil t)))

(defun piper-pandoc-convert (writer &optional buffer beginning end reader)
  "Output WRITER formatted string of BUFFER parsed using READER.

The file is parsed between BEGINNING and END."
  (let* ((buffer (or buffer (current-buffer)))
         (pandoc-buffer (get-buffer-create pandoc--output-buffer-name))
         (begginning (or beginning (point-min)))
         (end (or end (point-max)))
         (reader (or reader (cdr (assq major-mode pandoc-major-modes))))
         (text))
    (switch-to-buffer pandoc-buffer)
    (erase-buffer)
    (switch-to-buffer buffer)
    (call-process-region beginning end "pandoc" nil pandoc-buffer t
                         "--read"
                         reader
                         "--write"
                         writer
                         "--quiet"
                         "--wrap=none")
    (switch-to-buffer pandoc-buffer)
    (setq text (buffer-string))
    (bury-buffer)
    text))

(defun piper-plaintextify (beginning end)
  "Call pandoc to convert our buffer to plain text.

This kills links etc. which we typically do  not want read out.

Region between BEGINNING and END is converted."
  (let ((pandoc-use-async nil)
        (reader (cdr (assq major-mode pandoc-major-modes)))
        (text))
    (if reader
        (replace-regexp-in-string "_" ""
                                  (piper-pandoc-convert "plain" (current-buffer) beginning end reader))
      (buffer-substring beginning end))))

;; We may want to add some silence between paragraphs.

(defun piper-text-postprocess (text)
  "Postprocess TEXT before passing it to polly proper."
  (let ((tmp text))
    (setq tmp (replace-regexp-in-string "^> \\(.*\\)" "\\1" tmp))
    (setq tmp (replace-regexp-in-string "^- \\(.*\\)" "\\1" tmp))
    (setq tmp (replace-regexp-in-string "\\mat{\\(.*?\\)}" "\\1" tmp))
    (setq tmp (replace-regexp-in-string "\\vec{\\(.*?\\)}" "\\1" tmp))
    (setq tmp (replace-regexp-in-string "\\\\" "" tmp))
    (setq tmp (replace-regexp-in-string "\\$\\(.*?\\)\\$" "\\1" tmp))
    tmp))

;;;###autoload
(defun piper-region (arg)
  "Speak text with Piper.

When no region is active the current paragraph is used. When
prefix argument is given ask for voice first.

When ARG is given, allow to select the voice first."
  (interactive "P")
  (let* ((beginning (if (not (use-region-p))
                        (save-excursion (backward-paragraph) (point))
                      (region-beginning)))
         (end (if (not (use-region-p))
                  (save-excursion (forward-paragraph) (point))
                (region-end)))
         (texts (split-string (piper-plaintextify beginning end) "\n\n"))
         (silence (make-temp-file "emacs-piper-silence" nil ".mp3"))
         (files nil)
         (set-voice (if arg (piper-voices-completing-read) nil)))
    (dolist (text texts)
      (let* ((voice (if set-voice set-voice (piper-select-voice text)))
             (temp-filename (make-temp-file "emacs-piper"))
             (input-filename (concat temp-filename ".txt"))
             (output-filename (concat temp-filename ".wav"))
             (cmd (s-format piper-command 'aget
                            `(("input-filename" . ,input-filename)
                              ("voice" . ,voice)
                              ("output-filename" . ,output-filename)))))
        (f-write-text (piper-text-postprocess text) 'utf-8 input-filename)
        (call-process-shell-command cmd nil nil nil)
        (add-to-list 'files output-filename t)))
    ;; TODO I'm too lazy to figure out how to prevent emms from keeping th last file in the
    ;; playlist, we just add a short silence to make it unnoticeable
    (add-to-list 'files silence t)
    (dolist (file files)
      (let ((current-prefix-arg nil))
        (emms-add-file file)))
    (emms-start)))

(defcustom piper-make-silence-command
  "ffmpeg -f lavfi -y -i anullsrc=r=22050:cl=mono -t %f -q:a 9 -acodec libmp3lame %s"
  "Command to make moments of silence."
  :group 'piper
  :type 'string)

(defun piper-make-silence (length output-filename)
  "Write LENGTH seconds of silence to OUTPUT-FILENAME."
  (call-process-shell-command (format piper-make-silence-command length output-filename) nil nil nil))


(provide 'piper)
;;; piper.el ends here
