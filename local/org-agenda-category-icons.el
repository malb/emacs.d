;;; org-agenda-category-icons.el --- A configuration macro for defining your agenda icons  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Edward Minnix

;; Author: Edward Minnix III <egregius313@gmail.com>
;; Maintainer: Edward Minnix III <egregius313@gmail.com>
;; Created: 14 Apr 2022
;; Modified: 14 Apr 2022
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (dash "2.13") (all-the-icons "4.0.1"))
;; Keywords: dotemacs config org-mode agenda convenience
;; URL: https://github.com/jwiegley/use-package

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The `agenda-category-icons!' declaration macro can be used for defining the
;; `org-agenda-category-icon-alist' in a declarative style.

;; Please see README.org to read the documentation.

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'dash)
(require 'cl-lib)
(require 'all-the-icons)

(defgroup org-agenda-category-icons
  '()
  "Declarative definition for `org-agenda-category-icon-alist'."
  :group 'org-agenda)

(defun org-agenda-category-icons-identity (x)
  "Identity function. Return the value `X'."
  x)

(defcustom org-agenda-category-icons-iconset-alist
  '((:faicon . all-the-icons-faicon)
    (:material . all-the-icons-material)
    (:octicon . all-the-icons-octicon)
    (:fileicon . all-the-icons-fileicon)
    (:alltheicon . all-the-icons-alltheicon)
    (:wicon . all-the-icons-wicon)
    (:emoji . org-agenda-category-icons-identity))
  "Functions for defining agenda icons."
  :type '(alist :key-type keyword :value-type function))

(defmacro org-agenda-category-icons! (&rest specs)
  "Define the agenda icons specified by `SPECS'."
  (cl-flet* ((to-string (x)
              "Convert `X' to a string."
              (if (symbolp x) (symbol-name x) x))
             (expand-p (s)
              "Detemine if `S' needs to be evaluated, and if so, return the symbol to be evaluated."
              (when (and (symbolp s)
                         (->> s symbol-name (string-prefix-p "@")))
                (-> s
                    symbol-name
                    (substring 1)
                    intern)))
             (expand-categories (categories)
              "Expand any `categories' which need to be evaluated."
              (cl-loop for category in categories
                       for expand = (expand-p category)
                       if expand append (eval expand)
                       else collect category))
             (define-agenda-icon (category icon-set icon-name &optional regex)
               "Define an agenda icon, for `category' with `icon-name' from `icon-set'."
               (let* ((icon-function (alist-get icon-set org-agenda-category-icons-iconset-alist))
                      (icon (funcall icon-function icon-name))
                      (category-selector (if regex
                                             category
                                           (concat (rx bos) category (rx eos))))
                      (entry (list category-selector (list icon) nil nil :ascent 'center)))
                 (add-to-list 'org-agenda-category-icon-alist entry))))
    (let ((icon-set))
      (dolist (spec specs)
        (cond
         ((keywordp spec) (setf icon-set spec))
         (t
          (let ((icon-name (car spec))
                (categories (cdr spec)))
            (dolist (category (expand-categories categories))
              (define-agenda-icon (to-string category) icon-set (to-string icon-name))))))))))

(provide 'org-agenda-category-icons)
;;; org-agenda-category-icons.el ends here
