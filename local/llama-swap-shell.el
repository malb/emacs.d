;;; llama-swap-shell.el --- A shell for llama-swap -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; A simple chat interface to llama-swap using the OpenAI API.

;;; Code:

(require 'shell-maker)
(require 'markdown-overlays)
(require 'request)
(require 'prodigy)
(require 'yaml)

(defgroup llama-swap-shell nil
  "Chat to models via llama-swap."
  :group 'tools)

(defcustom llama-swap-shell-api-url-base "http://localhost:8080"
  "URL of lllama-swap server."
  :group 'llama-swap-shell
  :type 'string)

(defcustom llama-swap-shell-models nil
  "A list of models. The first entry is picked by default."
  :group 'llama-swap-shell
  :type '(repeat string))

(defcustom llama-swap-shell-pre-startup-functions '(llama-swap-shell-start-server-maybe)
  "Functions called before start, e.g. to start the server."
  :group 'llama-swap-shell
  :type '(list function))

(defcustom llama-swap-shell-prodigy-service-name nil
  "Name of the prodigy service for llama-swap if any."
  :group 'llama-swap-shell
  :type 'string)

(defcustom llama-swap-shell-streaming t
  "Stream responses."
  :group 'llama-swap-shell
  :type 'boolean)

(defcustom llama-swap-shell-default-model nil
  "Default model to pick."
  :group 'llama-swap-shell
  :type 'string)

(defvar llama-swap-shell-input-ring (make-ring 256))

(defvar llama-swap-shell--model nil)

(defvar-keymap llama-swap-shell-mode-map
  :parent shell-maker-mode-map
  :doc "Keymap for `llama-swap-shell-mode'."
  "C-r" #'shell-maker-clear-buffer)

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
              (progn (push filename images)
                     (delete-region beg end))
            (message "llama-swap: file not found: %s" filename))))
      (setq prompt (buffer-substring-no-properties (point-min) (point-max))))
    (cons prompt (nreverse images))))

(defun llama-swap-shell-start-server-maybe (&rest _)
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
;;;###autoload
(defun llama-swap-shell-models-from-server ()
  "Get list of models from the server."
  (mapcar (lambda (x) (alist-get 'id x))
          (alist-get 'data (request-response-data
                            (request
                              (format "%s/v1/models" llama-swap-shell-api-url-base)
                              :parser 'json-read
                              :sync t)))))

(defun llama-swap-shell-image-to-request (filename)
  "Turn image given by a FILENAME into base64 encoded request."
  `(:type "image_url"
    :image_url (:url
                ,(format "data:%s;base64,%s"
                         (s-replace "\n" "" (shell-command-to-string (format "xdg-mime query filetype %s" filename)))
                         (with-temp-buffer
                           (insert-file-contents filename)
                           (base64-encode-string (buffer-string) t))))))

(defun llama-swap-shell-select-model (&optional use-cache)
  "Interactively select a model, USE-CACHE if instructed."
  (interactive "P")
  (unless use-cache
    (setq llama-swap-shell-models (llama-swap-shell-models-from-server)))
  (setq llama-swap-shell--model (completing-read "Llama Swap Model: " llama-swap-shell-models)))

(defun llama-swap-shell-make-a-message (prompt)
  "Parse PROMPT and prepare data structure for submission."
  (let* ((parsed (llama-swap-shell-base64-images-maybe prompt))
         (prompt (car parsed))
         (images (cdr parsed))
         content)
    (push `(:type "text" :text ,prompt) content)
    (dolist (image images)
      (push (llama-swap-shell-image-to-request image) content)
      (message "Attached %s" image))
    `(:role "user" :content ,(vconcat (nreverse content)))))

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
       (when (car item) (push (llama-swap-shell-make-a-message (car item)) result))
       (when (cdr item) (push `(:role "assistant" :content ,(cdr item)) result)))
     history)
    result))

(defun llama-swap-shell-create-request (_ model)
  "Create request from PROMPT and history to MODEL."
  (let* ((messages (llama-swap-shell-make-messages)))
    `(:model ,model
      :messages ,(vconcat (nreverse messages))
      :stream ,llama-swap-shell-streaming)))

(defun llama-swap-shell-parse-response (raw-response)
  "Parse RAW-RESPONSE."
  (let ((chunks (split-string (map-elt raw-response :pending) "\n"))
        response)

    (setf (map-elt raw-response :pending) nil)

    (if llama-swap-shell-streaming
        (dolist (chunk chunks)

          (let* ((chunk (replace-regexp-in-string "data: " "" chunk))
                 (json (alist-get 'choices (shell-maker--json-parse-string chunk))))
            (when json
              (setq json (aref json 0))
              (cond ((assoc 'role (alist-get 'delta json)) t)
                    (t (setq response (concat response (alist-get 'content (alist-get 'delta json)))))))))
      (dolist (chunk chunks)
        (let ((json (aref (alist-get 'choices (shell-maker--json-parse-string chunk)) 0)))
          (setq response (concat response (alist-get 'content (alist-get 'message json)))))))
    (list (cons :filtered (concat response)))))

(defun llama-swap-shell-call-api-curl-command (object)
  "Encode OBJECT as json and send to MODEL via curl."
  (let ((json (json-encode object))
        (json-path (make-temp-file "llama-swap-shell-request" nil ".json")))
    (with-temp-file json-path (insert json))
    (append (list "curl" (format "%s/v1/chat/completions" llama-swap-shell-api-url-base))
            `("--data" ,(format "@%s" json-path)))))

(defun llama-swap-shell-prompt-pair (model)
  "Return a prompt-for given MODEL."
  (cons
   (format "llama-swap(%s)> " model)
   (rx bol "llama-swap(" (minimal-match (one-or-more not-newline)) ")> ")))

;;;###autoload
(defun llama-swap-shell (&optional arg)
  "Start an llama-swap shell.

When ARG is given, prompt the user for a model."
  (interactive "P")

  (dolist (f llama-swap-shell-pre-startup-functions)
    (funcall f))

  (setq llama-swap-shell--model
        (cond
         (arg (llama-swap-shell-select-model))
         ((not llama-swap-shell--model) (if llama-swap-shell-default-model
                                            llama-swap-shell-default-model
                                          (car llama-swap-shell-models)))
         (t llama-swap-shell--model)))

  (let* ((prompt-pair (llama-swap-shell-prompt-pair llama-swap-shell--model))
         (initial-prompt (when (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))))
         (config (make-shell-maker-config
                  :name "llama-swap"
                  :prompt (car prompt-pair)
                  :prompt-regexp (cdr prompt-pair)
                  :execute-command
                  (lambda (command shell)
                    (ring-insert llama-swap-shell-input-ring command)
                    (shell-maker-execute-command
                     :async t
                     :command (llama-swap-shell-call-api-curl-command
                               (llama-swap-shell-create-request command llama-swap-shell--model))
                     :filter #'llama-swap-shell-parse-response
                     :shell shell))
                  :on-command-finished
                  (lambda (_ _ _) ;; (command output success)
                    (markdown-overlays-put)))))

    (shell-maker-start config)
    (shell-maker-set-prompt (car prompt-pair) (cdr prompt-pair))

    (setq comint-input-ring (ring-copy llama-swap-shell-input-ring))
    (when initial-prompt (insert initial-prompt))))

(provide 'llama-swap-shell)
;;; llama-swap-shell.el ends here
