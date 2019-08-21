;;; aws-polly --- Speak text with AWS Polly
;;; Commentary:
;;;
;;; A simple interface for AWS’ text-to-speech API. All text is sent to Amazon’s severs so keep
;;; privacy implications in mind before using this.

;;; Code:

(require 'emms)
(require 'guess-language)
(require 'pandoc-mode-utils)

(defgroup aws-polly nil
  "Run AWS Polly for Text to Speech"
  :group 'tools)

;; AWS offers various voices to choose from

(defcustom aws-polly-voices '(("Emma"     "en-GB") ("Brian"     "en-GB") ("Joanna"    "en-US") ("Mizuki"   "ja-JP")
                              ("Filiz"    "tr-TR") ("Astrid"    "sv-SE") ("Maxim"     "ru-RU") ("Tatyana"  "ru-RU")
                              ("Carmen"   "ro-RO") ("Ines"      "pt-PT") ("Cristiano" "pt-PT") ("Vitoria"  "pt-BR")
                              ("Ricardo"  "pt-BR") ("Maja"      "pl-PL") ("Jan"       "pl-PL") ("Ewa"      "pl-PL")
                              ("Ruben"    "nl-NL") ("Lotte"     "nl-NL") ("Liv"       "nb-NO") ("Giorgio"  "it-IT")
                              ("Carla"    "it-IT") ("Karl"      "is-IS") ("Dora"      "is-IS") ("Mathieu"  "fr-FR")
                              ("Celine"   "fr-FR") ("Chantal"   "fr-CA") ("Penelope"  "es-US") ("Miguel"   "es-US")
                              ("Enrique"  "es-ES") ("Conchita"  "es-ES") ("Geraint"   "en-GB-WLS") ("Salli" "en-US")
                              ("Kimberly" "en-US") ("Kendra"    "en-US") ("Justin"    "en-US") ("Joey"     "en-US")
                              ("Ivy"      "en-US") ("Raveena"   "en-IN")  ("Amy"      "en-GB") ("Russell"  "en-AU")
                              ("Nicole"   "en-AU") ("Marlene"   "de-DE") ("Hans"      "de-DE") ("Naja"     "da-DK")
                              ("Mads"     "da-DK") ("Gwyneth"  " cy-GB") ("Jacek"     "pl-PL"))
  "Voices to use in AWS polly in order of preference."
  :group 'aws-polly
  :type ''(alist :value-type (string string)))

;; We call the command line client which can be obtained by =pip install awscli=

(defcustom aws-polly-command "aws polly synthesize-speech --output-format mp3 --voice-id %s --text \"%s\" %s"
  "Command to run AWS polly."
  :group 'aws-polly
  :type 'string)

(defun aws-polly-is-quote (text)
  "Decide if TEXT is a quote."
  (equal (and (>= (length text) 2) (substring text 0 2)) "  "))

;; Return the first voice matching detected language.

(defun aws-polly-select-voice (text)
  "Select voice by picking first voice from aws-polly-voices matching detected language in TEXT."
  (let ((lang (with-temp-buffer
                (insert text)
                (replace-regexp-in-string "_" "-" (cadr (assq (guess-language-buffer) guess-language-langcodes)))))
        (voices nil))
    (dolist (entry aws-polly-voices)
      (if (string-prefix-p lang (cadr entry))
          (push (car entry) voices)))
    (setq voices (nreverse voices))
    (cond
     ((eq (length voices) 0) (caar aws-polly-voices))
     ((eq (length voices) 1) (car voices))
     (t (if (aws-polly-is-quote text)
            (cadr voices)
          (car voices))))))

(defun aws-polly-voices-completing-read ()
  "Offer list of AWS Polly voices to choose from and return choice."
  (replace-regexp-in-string
   " .*$" "" (completing-read "Voice: "
                              (mapcar (lambda (x) (format "%s (%s)" (car x) (cadr x)))
                                      aws-polly-voices) nil t)))

;; AWS Polly will not read any text longer than 1500 characters as of writing.

(defvar aws-polly-character-limit 1500
  "Number of characters accepted by AWS polly.")

(defun aws-polly-plaintextify (beginning end)
  "Call pandoc to convert our buffer to plain text.

This kills links etc. which we typically do  not want read out.

Region between BEGINNING and END is converted."
  (let ((pandoc-use-async nil)
        (reader (cdr (assq major-mode pandoc-major-modes)))
        (text))
    (if reader
        (replace-regexp-in-string "_" ""
                                  (malb/pandoc-convert "plain" (current-buffer) beginning end reader))
      (buffer-substring beginning end))))

;; We may want to add some silence between paragraphs.

(defcustom aws-polly-make-silence-command
  "ffmpeg -f lavfi -y -i anullsrc=r=22050:cl=mono -t %f -q:a 9 -acodec libmp3lame %s"
  "Command to make moments of silence."
  :group 'aws-polly
  :type 'string)

(defun aws-polly-make-silence (length output-filename)
  "Write LENGTH seconds of silence to OUTPUT-FILENAME."
  (call-process-shell-command (format aws-polly-make-silence-command length output-filename) nil nil nil))

;;;###autoload
(defun aws-polly-region (arg)
  "Speak text with AWS polly.

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
         (texts (split-string-and-unquote (aws-polly-plaintextify beginning end) "\n\n"))
         (silence (make-temp-file "emacs-aws-polly-silence" nil ".mp3"))
         (files nil))
    (aws-polly-make-silence 1.0 silence)
    (dolist (text texts)
      (if (> (length text) aws-polly-character-limit)
          (error "AWS polly will only accept up 1500 characters but got %d \"%s\"" (length text) (substring text 0 32))))
    (dolist (text texts)
      (let ((voice (if arg (aws-polly-voices-completing-read)
                     (aws-polly-select-voice text)))
            (output-filename (make-temp-file "emacs-aws-polly" nil ".mp3")))
        (call-process-shell-command (format aws-polly-command voice text output-filename) nil nil nil)
        (setq files (append files (list output-filename silence)))))
    (dolist (file files)
      (emms-add-file file))
    (emms-start)))

(provide 'aws-polly)
;;; aws-polly.el ends here
