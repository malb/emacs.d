;;; helm-ddgr.el --- Emacs Helm Interface for quick DuckDuckGo searches (ddgr)

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

;; Emacs Helm Interface for quick DuckDuckGo searches (using ddgr). Based on helm-google.

;;; Code:

(require 'helm)
(require 'helm-net)
(require 'json)
(require 'org)

(defgroup helm-ddgr '()
  "Customization group for `helm-ddgr'."
  :group 'convenience
  :group 'comm)

(defcustom helm-ddgr-actions
  '(("Browse URL" . browse-url)
    ("Browse URL with EWW" . (lambda (candidate)
                               (eww-browse-url candidate)))
    ("Org Store Link" . (lambda (candidate)
                          (push (list candidate candidate) org-stored-links))))
  "List of actions for helm-ddgr sources."
  :group 'helm-ddgr
  :type '(alist :key-type string :value-type function))

(defcustom helm-ddgr-binary "ddgr"
  "Ddgr binary."
  :type 'string)

(defcustom helm-ddgr-idle-delay 0.5
  "Time to wait when idle until query is made."
  :type 'integer
  :group 'helm-ddgr)

(defvar helm-ddgr-input-history nil)
(defvar helm-ddgr-pending-query nil)

(defun helm-ddgr--search (text)
  "Fetch the response buffer for input TEXT and parse it as a plist."
  (with-temp-buffer
    (call-process-shell-command (format "%s --json %s" helm-ddgr-binary
                                        (shell-quote-argument text)) nil t)
    (goto-char (point-min))
    (let ((json-object-type 'plist))
      (json-read))))

(defun helm-ddgr-search ()
  (let* ((results (helm-ddgr--search helm-pattern)))
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
(defun helm-ddgr (&optional search-term)
  "Web search interface for Emacs."
  (interactive)
  (let ((input (or search-term (when (use-region-p)
                                 (buffer-substring-no-properties
                                  (region-beginning)
                                  (region-end))))))
    (helm :sources `((name . "Ddgr")
                     (action . helm-ddgr-actions)
                     (candidates . helm-ddgr-search)
                     (requires-pattern)
                     (nohighlight)
                     (multiline)
                     (match . identity)
                     (volatile))
          :prompt "DuckDuckGo: "
          :input input
          :input-idle-delay helm-ddgr-idle-delay
          :buffer "*helm ddgr*"
          :history 'helm-ddgr-input-history)))

(provide 'helm-ddgr)

;;; helm-ddgr.el ends here
