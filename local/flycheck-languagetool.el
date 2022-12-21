;;; flycheck-languagetool.el --- Check style and grammer using Flycheck and LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Package-Requires: ((emacs "25.1") (flycheck "31"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'flycheck)
(require 'langtool)

(defun flycheck-languagetool--parser (output checker buffer)
  (mapcar
   (lambda (match)
     (let-alist match
       (flycheck-error-new-at
        (line-number-at-pos (1+ .offset))
        (save-excursion
          (goto-char (1+ .offset))
          ;; Flycheck 1-base, Emacs 0-base
          (1+ (current-column)))
        'warning
        .message
        :id .rule.id
        :checker checker
        :buffer buffer
        :filename (buffer-file-name buffer))))
   (alist-get 'matches (car (flycheck-parse-json output)))))

(flycheck-define-checker languagetool
  "Style and grammar checker using LanguageTool."
  :command ("java"
            (option "-jar" langtool-language-tool-jar)
            "-d" (eval (mapconcat 'identity langtool-disabled-rules ","))
            ;; (option "-l" langtool-default-language)
            "--json"
            "-")
  :standard-input t
  :error-parser flycheck-languagetool--parser
  :modes (text-mode org-mode markdown-mode)
  :predicate (lambda ()
               (and langtool-language-tool-jar
                    (file-exists-p langtool-language-tool-jar)))
  :verify (lambda (_)
            (let ((have-jar
                   (and langtool-language-tool-jar
                        (file-exists-p langtool-language-tool-jar))))
              (list
               (flycheck-verification-result-new
                :label (or langtool-language-tool-jar
                           "languagetool-commandline.jar")
                :message (if have-jar "exist" "doesn't exist")
                :face (if have-jar 'success '(bold error)))))))

;;;###autoload
(defun flycheck-languagetool-setup ()
  "Setup Flycheck LanguageTool."
  (add-to-list 'flycheck-checkers 'languagetool))

(provide 'flycheck-languagetool)
;;; flycheck-languagetool.el ends here
