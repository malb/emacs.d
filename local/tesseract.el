;;; tesseract.el --- Tesseract from Emacs

;;; Commentary:
;; This package provides an interface similar to the Mathpix snipping tool.
;;
;; tesseract-screenshot prompts for a screenshot, which is processed by tesseract.
;;;;
;; Heavily adapted from mathpix.
;;

;;; Commentary:
;;; Code:

(require 'json)

(defgroup tesseract nil
  "OCR with tesseract."
  :group 'tools)

;; From org-download
(defcustom tesseract-screenshot-method "gnome-screenshot -a -f %s"
  "The tool to capture screenshots."
  :group 'tesseract
  :type '(choice
          (const :tag "gnome-screenshot" "gnome-screenshot -a -f %s")
          (const :tag "scrot" "scrot -s %s")
          (const :tag "gm" "gm import %s")
          (const :tag "imagemagick/import" "import %s")
          (const :tag "imagemagick/import + xclip to save to clipboard"
                 "export filename=\"%s\"; import png:\"$filename\" ;xclip -selection clipboard -target image/png -filter < \"$filename\" &>/dev/null")
          (const :tag "xfce4-screenshooter" "xfce4-screenshooter -r -o cat > %s")
          ;; screenshot method in ms-windows, /capture=4 stands for interactive.
          (const :tag "IrfanView" "i_view64 /capture=4 /convert=\"%s\"")
          ;; screenshot script in osx, -i stands for interactive,
          ;; press space key to toggle between selection and
          ;; window/application mode.
          (const :tag "screencapture" "screencapture -i %s")
          ;; take an image that is already on the clipboard, for Linux
          (const :tag "xclip"
                 "xclip -selection clipboard -t image/png -o > %s")
          ;; take an image that is already on the clipboard, for Windows
          (const :tag "imagemagick/convert" "convert clipboard: %s")
          (function :tag "Custom function")))

(defcustom tesseract-screenshot-file
  (expand-file-name "tesseract.png" temporary-file-directory)
  "The file to capture tesseract screenshots."
  :group 'tesseract
  :type 'string)

(defconst tesseract-command
  "tesseract %s %s"
  "The shell executable command to retrieve the results.")

;; screenshot programs have exit-code of 0 even when screenshotting is cancelled.
;; To save API calls, we use the existence of the file as a check if the user
;; wants to continue. Hence, we must delete the file after each function call.
;;;###autoload
(defun tesseract-screenshot ()
  "Capture screenshot and send result to Tesseract."
  (interactive)
  (let ((default-directory "~"))
    (make-directory (file-name-directory tesseract-screenshot-file) t)
    (if (functionp tesseract-screenshot-method)
        (funcall tesseract-screenshot-file tesseract-screenshot-file)
      (shell-command-to-string
       (format tesseract-screenshot-method tesseract-screenshot-file)))
    (when (file-exists-p tesseract-screenshot-file)
      (let ((latex (tesseract-get-result tesseract-screenshot-file)))
        (kill-new latex)
        (insert latex))
      (delete-file tesseract-screenshot-file))))

(defun tesseract-get-result (file)
  "Sends the image FILE to Tesseract."
  (let* ((temp-filename (make-temp-file "tesseract"))
         (command (format tesseract-command file temp-filename)))
    (shell-command command)
    (message "%s" temp-filename)
    (with-temp-buffer
      (insert-file-contents (concat temp-filename ".txt"))
      (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'tesseract)

;;; tesseract.el ends here
