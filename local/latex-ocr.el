;;; latex-ocr.el --- MathPix-like interface from Emacs

;;; Commentary:
;; This package provides an interface similar to the Mathpix snipping tool.
;;
;; latex-ocr-screenshot prompts for a screenshot, which is processed by some tool outputting LaTeX on stdout.
;;
;; Examples:
;; - https://github.com/lukas-blecher/LaTeX-OCR
;; - https://github.com/Ucas-HaoranWei/GOT-OCR2.0/
;;
;; Heavily adapted from mathpix.
;;

;;; Commentary:
;;; Code:

(require 'json)

(defgroup latex-ocr nil
  "LaTeX pictures to LaTeX source code."
  :group 'tools)

;; From org-download
(defcustom latex-ocr-screenshot-method "gnome-screenshot -a -f %s"
  "The tool to capture screenshots."
  :group 'latex-ocr
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

(defcustom latex-ocr-screenshot-file
  (expand-file-name "latex-ocr.png" temporary-file-directory)
  "The file to capture latex-ocr screenshots."
  :group 'latex-ocr
  :type 'string)

(defconst latex-ocr-command
  "pix2tex %s"
  "The shell executable command to retrieve the results.")

;; screenshot programs have exit-code of 0 even when screenshotting is cancelled.
;; To save calls, we use the existence of the file as a check if the user
;; wants to continue. Hence, we must delete the file after each function call.
;;;###autoload
(defun latex-ocr-screenshot ()
  "Capture screenshot and send result to Latex-Ocr."
  (interactive)
  (let ((default-directory "~"))
    (make-directory (file-name-directory latex-ocr-screenshot-file) t)
    (if (functionp latex-ocr-screenshot-method)
        (funcall latex-ocr-screenshot-method latex-ocr-screenshot-file)
      (shell-command-to-string
       (format latex-ocr-screenshot-method latex-ocr-screenshot-file)))
    (when (file-exists-p latex-ocr-screenshot-file)
      (let ((latex (latex-ocr-get-result latex-ocr-screenshot-file)))
        (kill-new latex)
        (insert latex))
      (delete-file latex-ocr-screenshot-file))))

(defun latex-ocr-get-result (file)
  "Sends the image FILE to the latex to ocr engine."
  (let* ((command (format latex-ocr-command file))
         (result (replace-regexp-in-string
                  ".*: \\(.*\\)$" "\\1"
                  (shell-command-to-string command))))
    result))

(provide 'latex-ocr)

;;; latex-ocr.el ends here
