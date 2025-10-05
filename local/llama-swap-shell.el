;;; llama-swap-shell.el --- A shell for llama-swap -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; A simple chat interface to llama-swap using the OpenAI API.

;;; Code:

(require 'shell-maker)
(require 'prodigy)
(require 'yaml)

(defgroup llama-swap-shell nil
  "Chat to models via llama-swap."
  :group 'tools)

(defcustom llama-swap-shell-api-url "http://localhost:8080/upstream/%s/chat/completions"
  "URL of lllama-swap server."
  :group 'llama-swap-shell
  :type 'string)

(defcustom llama-swap-shell-models nil
  "A list of models. The first entry is picked by default."
  :group 'llama-swap-shell
  :type '(list string))

(defcustom llama-swap-shell-pre-startup-functions '(llama-swap-shell-start-server-maybe)
  "Functions called before start, e.g. to start the server."
  :group 'llama-swap-shell
  :type '(list function))

(defcustom llama-swap-shell-prodigy-service-name nil
  "Name of the prodigy service for llama-swap if any."
  :group 'llama-swap-shell
  :type 'string)

(defvar llama-swap-shell-input-ring (make-ring 256))

(defvar llama-swap-shell--model nil)

(defvar-keymap llama-swap-shell-mode-map
  :parent shell-maker-mode-map
  :doc "Keymap for `llama-swap-shell-mode'.")

(defun llama-swap-shell-base64-images-maybe (prompt)
  "Check for image paths in PROMPT and return  base64 encoded images if so."
  (let (images)
    (with-temp-buffer
      (insert prompt)
      (goto-char 0)
      (while (re-search-forward
              (rx (seq "/" (1+ not-newline) "/" (1+ not-newline) (or ".png" ".jpg" ".jpeg" ".webp")))
              nil t)
        (let ((filename (match-string 0))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (if (file-exists-p filename)
              (progn
                (push
                 (format "data:%s;base64,%s"
                         (s-replace "\n" "" (shell-command-to-string (format "xdg-mime query filetype %s" filename)))
                         (with-temp-buffer
                           (insert-file-contents filename)
                           (base64-encode-string (buffer-string) t)))
                 images)
                (delete-region beg end))
            (message "ollama: file not found: %s" filename))))
      (setq prompt (buffer-substring-no-properties (point-min) (point-max))))
    (cons prompt (nreverse images))))

(defun llama-swap-shell-start-server-maybe ()
  "Start server if not already running."
  (when llama-swap-shell-prodigy-service-name
    (let ((llama-swap-service (prodigy-find-service llama-swap-shell-prodigy-service-name)))
      (when (not (prodigy-service-started-p llama-swap-service))
        (prodigy-start-service llama-swap-service)
        (sit-for 2)))))

;;;###autoload
(defun llama-swap-shell-models-from-config (path)
  "Read list models from config file in PATH."
  (mapcar (lambda (x) (symbol-name (car x)))
          (alist-get 'models (yaml-parse-string (with-temp-buffer
                                                  (insert-file-contents (expand-file-name path))
                                                  (buffer-string))
                                                :object-type 'alist))))

(defun llama-swap-shell-image-to-request (image)
  "Turn base64 encoded IMAGE into request."
  `(:role "user" :content [(:type "image_url" :image_url (:url ,image))]))

(defun llama-swap-shell-select-model ()
  "Interactively select a model."
  (interactive)
  (completing-read "Llama Swap Model: " llama-swap-shell-models))

;; based on chatgpt-shell-openai--user-assistant-messages
(defun llama-swap-shell-make-messages ()
  "Convert history to OpenAI format, but in reverse order.

Sequence must be a vector for json serialisation.

For example:

 [
   ((role . \"assistant\") (content . \"world\"))
   ((role . \"user\") (content . \"hello\"))
 ]"
  (let ((history (shell-maker-history))
        (result))
    (mapc
     (lambda (item)
       (when (car item)
         (let* ((parsed (llama-swap-shell-base64-images-maybe (car item)))
                (prompt (car parsed))
                (images (cdr parsed)))
           (dolist (image images)
             (push (llama-swap-shell-image-to-request image) result))
           (push `(:role "user" :content ,prompt)
                 result)))
       (when (cdr item)
         (push `(:role "assistant" :content ,(cdr item))
               result)))
     history)
    result))

(defun llama-swap-shell-create-request (prompt)
  "Create request from PROMPT and history."
  (let* ((messages (llama-swap-shell-make-messages))
         (parsed (llama-swap-shell-base64-images-maybe prompt))
         (prompt (car parsed))
         (images (cdr parsed)))

    (if images
        (progn
          (dolist (image images) (push (llama-swap-shell-image-to-request image) messages))
          (push `(:role "user" :content ,prompt) messages))
      (push `(:role "user" :content ,prompt) messages))

    `(:messages ,(vconcat (nreverse messages))
      :stream t)))

(defun llama-swap-shell-parse-response (raw-response)
  "Parse RAW-RESPONSE."
  (let ((chunks (split-string (map-elt raw-response :pending) "\n"))
        response)

    (setf (map-elt raw-response :pending) nil)

    ;; ;;stream nil
    ;; (dolist (chunk chunks)
    ;;   (let ((json (aref (alist-get 'choices (shell-maker--json-parse-string chunk)) 0)))
    ;;     (setq response (concat response (alist-get 'content (alist-get 'message json))))
    ;;     (add-to-list 'llama-swap-shell-messages (alist-get 'message json))
    ;;     ))

    (dolist (chunk chunks)
      (let* ((chunk (replace-regexp-in-string "data: " "" chunk))
             (json (alist-get 'choices (shell-maker--json-parse-string chunk))))
        (when json
          (setq json (aref json 0))
          (cond
           ((assoc 'role (alist-get 'delta json)) t)
           (t
            (setq response (concat response (alist-get 'content (alist-get 'delta json)))))
           ))
        ))
    (list (cons :filtered (concat response)))))

(defun llama-swap-shell-call-api-curl-command (object model)
  "Encode OBJECT as json and send to MODEL via curl."
  (let ((json (json-encode object))
        (json-path (make-temp-file "llama-swap-shell-request" nil ".json")))
    (with-temp-file json-path (insert json))
    (append (list "curl" (format llama-swap-shell-api-url model))
            `("--data" ,(format "@%s" json-path)))))

;;;###autoload
(defun llama-swap-shell (&optional arg)
  "Start an llama-swap shell.

When ARG is given, prompt the user for a model."
  (interactive "P")

  (setq llama-swap-shell--model
        (cond
         (arg (llama-swap-shell-select-model))
         ((not llama-swap-shell--model) (car llama-swap-shell-models))
         (t llama-swap-shell--model)))

  (let* ((initial-prompt (when (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))))
         (config (make-shell-maker-config
                  :name "llama-swap"
                  :prompt (format "llama-swap(%s)> " llama-swap-shell--model)
                  :execute-command
                  (lambda (command shell)
                    (ring-insert llama-swap-shell-input-ring command)
                    (shell-maker-execute-command
                     :async t
                     :command (llama-swap-shell-call-api-curl-command
                               (llama-swap-shell-create-request command)
                               llama-swap-shell--model)
                     :filter #'llama-swap-shell-parse-response
                     :shell shell)))))

    (dolist (f llama-swap-shell-pre-startup-functions)
      (funcall f))

    (shell-maker-start config)

    (setq comint-input-ring (ring-copy llama-swap-shell-input-ring))

    (when initial-prompt (insert initial-prompt))))

(provide 'llama-swap-shell)
;;; llama-swap-shell.el ends here
