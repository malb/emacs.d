;;; pix2tex.el --- Pix2TeX API from Emacs

;;; Commentary:
;; This package provides an interface similar to the Mathpix snipping tool.
;;
;; pix2tex-screenshot prompts for a screenshot, which is processed by pix2tex.
;;
;; See https://github.com/lukas-blecher/LaTeX-OCR
;;
;; Heavily adapted from mathpix.
;;

;;; Commentary:
;;; Code:

(require 'json)

(defgroup pix2tex nil
  "LaTeX pictures to LaTeX source code."
  :group 'tools)

;; From org-download
(defcustom pix2tex-screenshot-method "gnome-screenshot -a -f %s"
  "The tool to capture screenshots."
  :group 'pix2tex
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

(defcustom pix2tex-screenshot-file
  (expand-file-name "pix2tex.png" temporary-file-directory)
  "The file to capture pix2tex screenshots."
  :group 'pix2tex
  :type 'string)

(defconst pix2tex-command
  "pix2tex %s"
  "The shell executable command to retrieve the results.")

;; screenshot programs have exit-code of 0 even when screenshotting is cancelled.
;; To save API calls, we use the existence of the file as a check if the user
;; wants to continue. Hence, we must delete the file after each function call.
;;;###autoload
(defun pix2tex-screenshot ()
  "Capture screenshot and send result to Pix2TeX."
  (interactive)
  (let ((default-directory "~"))
    (make-directory (file-name-directory pix2tex-screenshot-file) t)
    (if (functionp pix2tex-screenshot-method)
        (funcall pix2tex-screenshot-file pix2tex-screenshot-file)
      (shell-command-to-string
       (format pix2tex-screenshot-method pix2tex-screenshot-file)))
    (when (file-exists-p pix2tex-screenshot-file)
      (let ((latex (pix2tex-get-result pix2tex-screenshot-file)))
        (kill-new latex)
        (insert latex))
      (delete-file pix2tex-screenshot-file))))

(defun pix2tex-get-result (file)
  "Sends the image FILE to Pix2TeX."
  (let* ((command (format pix2tex-command file))
         (result (replace-regexp-in-string
                  ".*: \\(.*\\)$" "\\1"
                  (shell-command-to-string command))))
    result))

(provide 'pix2tex)

;;; pix2tex.el ends here
