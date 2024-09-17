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

(defcustom piper-default-language "en-GB"
  "Fallback language."
  :group 'piper
  :type 'string)

(defcustom piper-voices nil
  "Voices to use in Piper in order of preference.

These are tuples of (\"tag\" \"lang-code\" \"model-path\" \"speaker-number\")."

  :group 'piper
  :type '(alist :value-type (string string string integer)))

(defcustom piper-command "cat ${input-filename} | piper -m ${voice} -s ${speaker-number} -f ${output-filename}"
  "Command to run Piper."
  :group 'piper
  :type 'string)

(defun piper-is-quote (text)
  "Decide if TEXT is a quote."
  (equal (and (>= (length text) 2) (substring text 0 2)) "  "))

;; Return the first voice matching detected language.

(defun piper-guess-language (text &optional default-language)
  "Guess the language of TEXT and fall back to DEFAULT-LANGUAGE."
  (if (length< text guess-language-min-paragraph-length)
      (or default-language piper-default-language)
    (with-temp-buffer
      (insert text)
      (replace-regexp-in-string
       "_" "-"
       (cadr (assq (guess-language-buffer) guess-language-langcodes))))))

(defun piper-select-voice (text &optional default-language)
  "Select voice by picking first voice matching detected language in TEXT."
  (let ((lang (piper-guess-language text default-language))
        (voices nil))
    (dolist (entry piper-voices)
      (if (string-prefix-p lang (nth 1 entry))
          (push (cddr entry) voices)))
    (setq voices (nreverse voices))
    (cond
     ((eq (length voices) 0) (cddr (car piper-voices)))
     ((eq (length voices) 1) (nth 0 voices))
     (t (if (piper-is-quote text)
            (nth 1 voices)
          (nth 0 voices))))))

(defun piper-voices-completing-read ()
  "Offer list of installed Piper voices to choose from and return choice."
  (let ((string (s-split ":"
                         (replace-regexp-in-string
                          ".* (\\(.*?:.*?\\)).*" "\\1"
                          (completing-read "Voice: "
                                           (mapcar (lambda (x) (format
                                                                "%s (%s:%s) [%s]"
                                                                (nth 0 x) (nth 2 x) (nth 3 x) (nth 1 x)))
                                                   piper-voices)
                                           nil t)))))
    (list (nth 0 string) (string-to-number (nth 1 string)))))

(defun piper-pandoc-convert (writer &optional buffer beginning end reader)
  "Output WRITER formatted string of BUFFER parsed using READER.

The file is parsed between BEGINNING and END."
  (let* ((buffer (or buffer (current-buffer)))
         (pandoc-buffer (get-buffer-create pandoc--output-buffer-name))
         (begginning (or beginning (point-min)))
         (end (or end (point-max)))
         (reader (or reader (cdr (assq major-mode pandoc-major-modes))))
         (text))
    (with-current-buffer pandoc-buffer
      (erase-buffer))
    (with-current-buffer buffer
      (call-process-region beginning end "pandoc" nil pandoc-buffer t
                           "--read"
                           reader
                           "--write"
                           writer
                           "--quiet"
                           "--wrap=none"))
    (with-current-buffer pandoc-buffer
      (setq text (buffer-string)))
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
    (setq tmp (replace-regexp-in-string (rx ", d.h. ") ", das heißt " tmp))
    (setq tmp (replace-regexp-in-string (rx " i.e." (or "," " " " ")) " that is " tmp))
    (setq tmp (replace-regexp-in-string (rx " e.g." (or "," " " " "))" for example " tmp))
    tmp))

(defun piper-render-speech (text &optional voice default-language)
  "Render speech for TEXT with VOICE.

Return output filename."
  (let* ((voice (if voice voice (piper-select-voice text default-language)))
         (temp-filename (make-temp-file "emacs-piper"))
         (input-filename (concat temp-filename ".txt"))
         (output-filename (concat temp-filename ".wav"))
         (cmd (s-format piper-command 'aget
                        `(("input-filename" . ,input-filename)
                          ("voice" . ,(nth 0 voice))
                          ("speaker-number" . ,(nth 1 voice))
                          ("output-filename" . ,output-filename)))))
    (f-write-text (piper-text-postprocess text) 'utf-8 input-filename)
    (call-process-shell-command cmd nil nil nil)
    output-filename))

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
         (full-text (piper-plaintextify beginning end))
         (default-language (piper-guess-language full-text))
         (texts (split-string full-text "\n\n"))
         (silence (make-temp-file "emacs-piper-silence" nil ".mp3"))
         (files nil)
         (set-voice (if arg (piper-voices-completing-read) nil)))
    (dolist (text texts)
      (add-to-list 'files (piper-render-speech text set-voice default-language) t))
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
