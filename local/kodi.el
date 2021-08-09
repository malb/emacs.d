;;; kodi --- Control KODI from Emacs
;;; Commentary:
;;;

;;; Code:


(require 'hydra)
(require 's)
(require 'all-the-icons)

(defvar kodi-hosts nil)
(defvar kodi-debug nil)
(defvar kodi-now-playing-fmt "${artist} - ${track}. \"${title}\" on ${album} (${year})")
(defvar kodi-volume-delta 1)

(defun kodi-register-host (host &rest args)
  "Register a new Kodi HOST.

ARGS contains :username, :password and :url."
  (add-to-list 'kodi-hosts `(,host . ((username . ,(plist-get args :username))
                                      (password . ,(plist-get args :password))
                                      (url . ,(plist-get args :url))))))

(defun kodi-auth-string (host)
  "Render BasicAuth string for HOST."
  (let ((data (alist-get host kodi-hosts)))
    (format "Basic %s" (base64-encode-string (concat
                                              (alist-get 'username data)
                                              ":"
                                              (alist-get 'password data))))))

(defun kodi-url (host)
  "Get URL for HOST."
  (alist-get 'url (alist-get host kodi-hosts)))

(defun kodi-normalize-host (&optional host)
  "Pick first HOST if none is provided."
  (if host host (caar kodi-hosts)))

(defun kodi-host-helper (host interactively)
  "Maybe query user for HOST when called INTERACTIVELY."
  (cond
   ((and host interactively) ;; interactive with prefix
    (intern (completing-read "Host: " (mapcar (lambda (x) (car x)) kodi-hosts))))
   ((and (not host) interactively) ;; interactive without prefix
    (kodi-normalize-host))
   ((and host (not interactively)) ;; nointeractive with input
    host)
   ((and (not host) (not interactively)) ;; nointeractive with input
    (kodi-normalize-host))))

(defun kodi-send-command (host method &optional params interactive)
  "Send command to HOST using METHOD with PARAMS.

Maybe query the user for HOST when INTERACTIVE is set."
  (let* ((params (if params params "[0]"))
         (host (kodi-host-helper host interactive))
         (url (kodi-url host))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(kodi-auth-string host))))
         (url-request-data (format "{\"id\": 1, \"jsonrpc\": \"2.0\", \"method\": \"%s\", \"params\": %s}"
                                   method params)))

    (when kodi-debug
      (message "%s" url-request-extra-headers)
      (message url-request-data))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (let* ((json-array-type 'list)
             (result (alist-get 'result (json-read))))
        (when kodi-debug
          (message "result: %s" result))
        result))))

(defun kodi-play-pause (&optional host)
  "Toggle play/pause on HOST."
  (interactive "P")
  (kodi-send-command host "Player.PlayPause" nil (called-interactively-p 'interactive)))

(defun kodi-next (&optional host)
  "Skip to next song on HOST."
  (interactive "P")
  (kodi-send-command host "Player.GoTo" "[0, \"next\"]" (called-interactively-p 'interactive)))

(defun kodi-prev (&optional host)
  "Skip to previous song on HOST."
  (interactive "P")
  (kodi-send-command host "Player.GoTo" "[0, \"previous\"]" (called-interactively-p 'interactive)))

(defun kodi-get-volume (&optional host)
  "Return volume for HOST."
  (let ((result (kodi-send-command host "Application.GetProperties" "[[\"volume\", \"muted\"]]" nil)))
    (if (eq (alist-get 'muted result) 'json-false)
        nil
      (alist-get 'volume result))))

(defun kodi-volume-up (&optional host)
  "Increment volume on HOST by `kodi-volume-delta'."
  (interactive "P")
  (let* ((host (kodi-host-helper host (called-interactively-p 'interactive)))
         (old-volume (kodi-get-volume host))
         (new-volume (+ old-volume kodi-volume-delta)))
    (kodi-send-command host "Application.SetVolume" (format "[%d]" new-volume) nil)))

(defun kodi-volume-down (&optional host)
  "Decrement volume on HOST by `kodi-volume-delta'."
  (interactive "P")
  (let* ((host (kodi-host-helper host (called-interactively-p 'interactive)))
         (old-volume (kodi-get-volume host))
         (new-volume (- old-volume kodi-volume-delta)))
    (kodi-send-command host "Application.SetVolume" (format "[%d]" new-volume) nil)))

(defun kodi-now-playing (&optional host)
  "Return currently playing song on HOST formatted according to `kodi-now-playing-fmt'."
  (interactive "P")
  (let ((result (alist-get 'item
                           (kodi-send-command host "Player.GetItem"
                                              "[0,[\"title\",\"artist\",\"year\",\"album\",\"track\"]]"
                                              (called-interactively-p 'interactive)))))
    (setf (alist-get 'artist result) (s-join ", " (alist-get 'artist result)))
    (if (called-interactively-p 'interactive)
        (message "%s" (s-format kodi-now-playing-fmt 'aget result))
      (s-format kodi-now-playing-fmt 'aget result))))

(defhydra kodi nil
  "Kodi Media Centre"
  ("<up>"    kodi-volume-up (all-the-icons-faicon "volume-up" :face 'all-the-icons :v-adjust 0.01) :color red)
  ("<down>"  kodi-volume-down (all-the-icons-faicon "volume-down" :face 'all-the-icons :v-adjust 0.01) :color red)
  ("<left>"  kodi-prev (all-the-icons-faicon "backward" :face 'all-the-icons :v-adjust 0.01) :color blue)
  ("<right>" kodi-next (all-the-icons-faicon "forward" :face 'all-the-icons :v-adjust 0.01) :color blue)
  ("<RET>"   kodi-now-playing (all-the-icons-faicon "info" :face 'all-the-icons :v-adjust 0.01) :color blue)
  ("SPC"     kodi-play-pause(all-the-icons-faicon "play" :face 'all-the-icons :v-adjust 0.01) :color blue))

(provide 'kodi)
;;; kodi.el ends here
