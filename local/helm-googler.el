;;; helm-googler.el --- Emacs Helm Interface for quick Google searches (googler)

;; Copyright (C) 2014-2018, Steckerhalter
;; Copyright (C) 2019 Martin Albrecht

;; Author: malb
;; Package-Requires: ((helm "0"))
;; Keywords: helm google search

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs Helm Interface for quick Google searches (using googler). Based on helm-google.

;;; Code:

(require 'helm)
(require 'helm-net)
(require 'json)
(require 'org)

(defgroup helm-googler '()
  "Customization group for `helm-googler'."
  :group 'convenience
  :group 'comm)

(defcustom helm-googler-actions
  '(("Browse URL" . browse-url)
    ("Browse URL with EWW" . (lambda (candidate)
                               (eww-browse-url candidate)))
    ("Copy URL to clipboard" . (lambda (candidate)
                                 (kill-new  candidate)))
    ("Browse URL with webkit xwidget" . (lambda (candidate)
                                          (xwidget-webkit-browse-url candidate)))
    ("Copy URL" . (lambda (candidate) (let ((url
                                             (replace-regexp-in-string "https://.*q=\\(.*\\)\&sa=.*" "\\1" candidate)))
                                        (kill-new url))))
    ("Org Store Link" . (lambda (candidate)
                          (let ((title (car (split-string candidate "[\n]+")))
                                (url
                                 (replace-regexp-in-string "https://.*q=\\(.*\\)\&sa=.*" "\\1" candidate)))
                            (push (list url title) org-stored-links)))))
  "List of actions for helm-googler sources."
  :group 'helm-googler
  :type '(alist :key-type string :value-type function))

(defcustom helm-googler-binary "googler"
  "Googler binary."
  :type 'string)

(defcustom helm-googler-idle-delay 0.25
  "Time to wait when idle until query is made."
  :type 'integer
  :group 'helm-googler)

(defvar helm-googler-input-history nil)
(defvar helm-googler-pending-query nil)

(defun helm-googler--search (text)
  "Fetch the response buffer for input TEXT and parse it as a plist."
  (with-temp-buffer
    (call-process-shell-command (format "%s --json %s" helm-googler-binary text) nil t)
    (goto-char (point-min))
    (let ((json-object-type 'plist))
      (json-read))))

(defun helm-googler-search ()
  (let* ((results (helm-googler--search helm-pattern)))
    (mapcar (lambda (result)
              (cons
               (concat
                (propertize (plist-get result :title) 'face 'font-lock-variable-name-face)
                "\n"
                (plist-get result :abstract)
                "\n"
                (propertize (plist-get result :url) 'face 'link))
               (plist-get result :url)))
            results)))

;;;###autoload
(defun helm-googler (&optional search-term)
  "Web search interface for Emacs."
  (interactive)
  (let ((input (or search-term (when (use-region-p)
                                 (buffer-substring-no-properties
                                  (region-beginning)
                                  (region-end))))))
    (helm :sources `((name . "Googler")
                     (action . helm-googler-actions)
                     (candidates . helm-googler-search)
                     (requires-pattern)
                     (nohighlight)
                     (multiline)
                     (match . identity)
                     (volatile))
          :prompt "Google: "
          :input input
          :input-idle-delay helm-googler-idle-delay
          :buffer "*helm googler*"
          :history 'helm-googler-input-history)))

(provide 'helm-googler)

;;; helm-googler.el ends here
