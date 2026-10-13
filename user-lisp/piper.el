;;; piper --- Speak text with Piper  -*- lexical-binding: t; -*-
;;;
;;; See https://github.com/rhasspy/piper
;;;
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'emms-setup)
(require 'emms-source-file)
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
  :type '(repeat (list string string string integer)))

(defcustom piper-command "cat ${input-filename} | piper-tts -m ${voice} -s ${speaker-number} -f ${output-filename}"
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
         (beginning (or beginning (point-min)))
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
        (reader (cdr (assq major-mode pandoc-major-modes))))
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
    (setq tmp (replace-regexp-in-string (rx "*" (group (*? anything))  "*") "\\1" tmp))
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
         (texts (split-string full-text "\n\n" t))
         (set-voice (if arg (piper-voices-completing-read) nil))
         (files (cl-loop for text in texts
                         collect (piper-render-speech text set-voice default-language))))
    (when emms-player-playing-p
      (emms-stop)
      ;; EMMS' mpv player defers a new `loadfile' until the previous track's
      ;; "end-file" event arrives (mpv ignores "stop + loadfile"); if we start
      ;; again while a stop is still pending, that deferred `loadfile' can be
      ;; dropped when mpv goes idle, so the new speech never plays.  Wait until
      ;; the pending stop has settled before issuing the next `emms-start'.
      (cl-loop with deadline = (+ (float-time) 1.0)
               while (and (boundp 'emms-player-mpv-ipc-stop-command)
                          (eq emms-player-mpv-ipc-stop-command t)
                          (< (float-time) deadline))
               do (sit-for 0.05)))
    (emms-playlist-current-clear)
    (cl-loop for file in files
             do (emms-source-add 'emms-source-file file))
    (emms-playlist-current-select-first)
    (emms-start)))


(provide 'piper)
;;; piper.el ends here
