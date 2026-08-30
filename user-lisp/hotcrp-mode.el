;;; hotcrp-mode.el --- major mode HotCRP offline reviewing

;; Copyright 2010 David Mazieres (http://www.scs.stanford.edu/~dm/)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License (GPL) as
;; published by the Free Software Foundation from
;; <http://www.gnu.org/licenses/>; you may choose either version 2 or
;; any later version of the GPL.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; Author: David Mazieres
;; Updated 3/20/21: Adam Doupe
;; Version: 0.1

;;; Comentary:
;;
;; More information on HotCRP is available here:
;;   http://www.cs.ucla.edu/~kohler/hotcrp/
;;
;; To use, add this to your .emacs:
;;
;; (autoload 'hotcrp-mode "hotcrp-mode" nil t)
;; (autoload 'hotcrp-fetch "hotcrp-mode" nil t)
;; (add-to-list 'magic-mode-alist
;;	     '("\\`==\\+== .* Review Form" . hotcrp-mode))
;;
;; Then use M-x hotcrp-fetch to get your offline review form.
;;
;; There might be a more recent version of this file at:
;; <http://www.scs.stanford.edu/~dm/hotcrp-mode/>

;;; Code:

(defvar hotcrp-default-url nil
  "*default HotCRP server from which to fetch files")
(defvar hotcrp-email nil
  "*Email address to use when logging into HotCRP systems")
(defvar hotcrp-pasword nil
  "*Password to use when logging into HotCRP systems, or nil to prompt")
(defvar hotcrp-insecure t
  "*Don't check SSL certificates")
(defvar hotcrp-view-pdf-program nil
  "*If non-nil, program to run to view PDF files.  If nil, use web browser.")
(defvar hotcrp-cookies (expand-file-name "~/.hotcrp")
  "File name in which to store HotCRP cookies")
(defvar hotcrp-curl "curl")

;;
;; Internal-use only:
;;
(defvar hotcrp-num-hist nil)
(defvar hotcrp-url-hist nil)
(defvar hotcrp-paper-cache nil)

(defun hotcrp-curl-args ()
  (append (if hotcrp-insecure '("-k") nil)
	  (list "-sSf" "-L" "-b" hotcrp-cookies "-c" hotcrp-cookies)))

(defconst hotcrp-paper-help-echo
  (eval-when-compile
    (concat "\\<hotcrp-paper-map>"
	    "\\[hotcrp-paper-click-reviews] for reviews, "
	    "\\[hotcrp-paper-click-paper] for paper."))) 

(defconst hotcrp-font-lock-keywords
  '(
    ("==\\(\\+\\|\\*\\)== .*"
     (0 '(face font-lock-type-face hotcrp-read-only t)))
    ("^==\\+== \\(Paper #\\([0-9]+\\)\\)"
     (1 (let* ((paper (match-string-no-properties 2)))
	  `(face font-lock-keyword-face
		 hotcrp-display (height 1.25)
		 hotcrp-mouse-face highlight
		 hotcrp-paper ,paper
		 hotcrp-help-echo
		   (substitute-command-keys hotcrp-paper-help-echo)
		 hotcrp-local-map ,hotcrp-paper-map))
	t))
    ("^==-==.*" (0 '(face font-lock-comment-face)))))

(defun hotcrp-url-encode (str)
  (mapconcat
   (lambda (c)
     (let ((s (string c)))
       (cond
	((eq c ?\n) "%0D%0A")
	((string-match "[-a-zA-Z0-9_:/.]" s) s)
	((eq c ?\s) "+")
	(t (format "%%%02x" c)))))
   str ""))

(defun hotcrp-normalize-url (url extension)
  (if (and extension (string-match "\\`[a-zA-Z]+://" extension))
      extension
    (unless (string-match "\\`[a-zA-Z]+://" url)
      (if (string-match "\\`[a-zA-Z0-9]" url)
	  (setq url (concat "http://" url))
	(error "Invalid URL `%s'" url)))
    (unless (string-match "://.*/" url)
      (setq url (concat url "/")))
    (if (not extension)
	url
      (if (eq (aref url 0) ?/)
	  (string-match "[^:]*://[^/]*" url)
	(string-match ".*/" url))
      (concat (match-string 0 url) extension))))

(defun hotcrp-curl-stdin (in url &optional extension &rest curl-args)
  (let* ((dfm (default-file-modes))
	 (buf (current-buffer))
	 ;;(display (not (string-match "\\` " (buffer-name buf))))
	 (display nil)
	 tmpbuf args ret)
    (or (not in) (stringp in) (bufferp in) (eq in 'async)
	(error "hotcrp-curl-stdin `%s' wrong type" in))
    (or url (setq url hotcrp-default-url))
    (or url (error "No HotCRP URL"))
    (setq url (hotcrp-normalize-url url extension))
    (unwind-protect
	(progn
	  (message "Fetching %s..." url)
	  (set-default-file-modes (logand dfm #o700))
	  (setq args (append (hotcrp-curl-args) curl-args (list url)))
	  (unless (eq 0
		      (cond
		       ((eq in 'async)
			(setq ret (apply 'start-process hotcrp-curl
					 (generate-new-buffer " *curl*")
					 hotcrp-curl args))
			0)
		       ((not in)
			(apply 'call-process hotcrp-curl nil t display args))
		       (t
			(if (bufferp in)
			    (set-buffer in)
			  (setq tmpbuf (generate-new-buffer " *hotcrpin*"))
			  (set-buffer tmpbuf)
			  (insert in))
			(apply 'call-process-region (point-min) (point-max)
			       hotcrp-curl tmpbuf buf display args))))
	    (unless (with-current-buffer buf (eq (point-min) (point-max)))
	      (display-buffer buf))
	    (error "Error fetching `%s'" url))
	  (unless (eq in 'async) (message nil))
	  ret)
      (set-default-file-modes dfm)
      (set-buffer buf)
      (if tmpbuf (kill-buffer tmpbuf)))))

(defun hotcrp-curl (&rest args)
  (apply 'hotcrp-curl-stdin nil args))

(defun hotcrp-must-login ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (re-search-forward "<input[^>]* type=['\"]password['\"]" nil t)))

(defun hotcrp-login (url)
  "Obtain cookies for accessing a HotCRP review web site.

First checks with the server to see if the current cookies are
still valid, and if not prompts the user for an email address and
password.  If the variable hotcrp-email is set, it is used rather
than prompt for an email address.  Similarly, if hotcrp-password
is set, then this value is used rather than prompting, but to use
this feature you must store passwords in initialization files,
which is not recommended.  (hotcrp-login stores cookies in the
file designated by variable hotcrp-cookies, so even without
setting hotcrp-password, you should only need to login once per
session.)"
  (let ((email hotcrp-email)
	(password hotcrp-pasword))
    (unwind-protect
	(with-current-buffer (get-buffer-create " *hotcrp*")
	  (kill-region (point-min) (point-max))
	  (setq url (hotcrp-normalize-url url "index.php"))
	  (hotcrp-curl url)
	  (when (hotcrp-must-login)
	    (unless email
	      (setq email (read-string "HotCRP Email: ")))
	    (unless password
	      (setq password (read-passwd "HotCRP Password: ")))
	    (if (string= password "") (error "Authentication failed"))
	    (setq password (concat "password=" (hotcrp-url-encode password)))
	    (kill-region (point-min) (point-max))
	    (hotcrp-curl-stdin password url "index.php"
			 "--data-urlencode" (concat "email=" email)
			 "-d@-" "-d" "action=login"
			 "--data-urlencode" "signin=Sign in")
	    (if (hotcrp-must-login) (error "Authentication failed"))
	    (if (and (not hotcrp-default-url) url)
		(setq hotcrp-default-url url))))
      (if password (clear-string password)))))

(defun hotcrp-clean (b ee)
  "Remove trailing carriage returns at the end of lines in region."
  (interactive "r")
  (save-excursion
    (goto-char b)
    (let ((e (copy-marker ee)))
      (while (re-search-forward "\r$" e t)
	(replace-match ""))
      (set-marker e nil))))

;;;###autoload
(defun hotcrp-fetch (&optional paper url)
  "Fetcn an offline review form from a HotCRP web server.

This function should usually be called in an empty buffer, as it
fills the buffer with the review form and switches the buffer's
major mode to hotcrp-mode (which works best when the review form
is the first thing in the buffer).

PAPER specifies the number of the paper to fetch.  It can also be
\"all\" to fetch all papers you are assigned to review, or
\"incomplete\" to fetch all papers you have been assigned to
review and for which you have not yet submitted reviews.

On the first invocation, if hotcrp-default-url is set,
hotcrp-fetch uses that as the URL of the server.  Otherwise,
hotcrp-fetch prompts for a URL.  On subsequent invocations,
hotcrp-fetch uses whatever value of URL was used in the previous
invocation.  When invoked with a prefix argument, however,
hotcrp-fetch always prompts for the URL."
  (interactive
   (list (let ((p (read-string
		   "Paper number, all, or incomplete (default incomplete): "
		   nil 'hotcrp-num-hist "incomplete")))
	   (cond
	    ((not p) nil)
	    ((string= p "incomplete") nil)
	    ((string= p "all") 'all)
	    ((string-match "\\`[0-9]+\\'" p) (string-to-number p))
	    (t (error "Invaid paper number `%s'" p))))
	 (let ((u (or (car hotcrp-url-hist) hotcrp-default-url)))
	   (or (and (not current-prefix-arg) u)
	       (read-string "HotCRP URL: " u 'hotcrp-url-hist u)))))
  (hotcrp-login url)
  (let ((inhibit-read-only t)
	(s (point)) e
	(ext (cond
	      ((not paper) "search?get=revform&q=&t=rout&pap=all")
	      ((eq paper 'all) "search?get=revform&q=&t=r&pap=all")
	      ((wholenump paper)
	       (format "search?get=revform&q=&t=r&pap=%d" paper))
	      (t (error "Invaid paper number `%s'" paper)))))
    (hotcrp-curl url ext)
    (hotcrp-clean s (point))
    (setq e (point))
    (goto-char s)
    (unless (looking-at "==\\+== ")
      (kill-region s e)
      (error "No papers")))
  (unless (eq major-mode 'hotcrp-mode) (hotcrp-mode)))

(defun hotcrp-browse-paper-url (&optional paper)
  "Browse the home page of paper number PAPER."
  (interactive "P")
  (setq paper
	(if paper
	    (format "%s" paper)
	  (completing-read "Paper number: "
			   (save-excursion (hotcrp-number-index))
			   nil nil nil 'hotcrp-num-hist)))
  (if (string= paper "")
      (error "Must specify paper number"))
  (browse-url (hotcrp-url-of-paper paper)))

(defun hotcrp-view (file)
  (cond
   ((not hotcrp-view-pdf-program)
    (browse-url file))
   ((stringp hotcrp-view-pdf-program)
    (call-process hotcrp-view-pdf-program nil 0 nil file))
   ((listp hotcrp-view-pdf-program)
    (apply 'call-process hotcrp-view-pdf-program nil 0 nil
	   (append hotcrp-view-pdf-program (list file))))
   (t
    (message "Paper downloaded to %s" file))))

(defun hotcrp-view-sentinel (cache file proc ev)
  (let ((status (process-status proc)) 
	(exit-status (process-exit-status proc))
	(buf (process-buffer proc)))
    (when (or (eq status 'exit) (not (eq exit-status 0)))
      (delete-process proc)
      (if (not (eq exit-status 0))
	  (progn
	    (setq hotcrp-paper-cache (delq cache hotcrp-paper-cache))
	    (when buf
	      (if (with-current-buffer buf (eq (point-min) (point-max)))
		  (kill-buffer buf)
		(display-buffer buf)))
	    (delete-file file)
	    (message "%s: %s" hotcrp-curl ev))
	(setcdr cache file)
	(hotcrp-view file)
	(and buf (kill-buffer buf))))))

(defun hotcrp-fetch-pdf (&optional paper)
  "Fetch and view a paper number PAPER on the hotcrp server."
  (interactive "P")
  (setq paper
	(if paper
	    (format "%s" paper)
	  (completing-read "Paper number: "
			   (save-excursion (hotcrp-number-index))
			   nil nil nil 'hotcrp-num-hist)))
  (if (string= paper "")
      (error "Must specify paper number"))

  (let* ((url (hotcrp-url-of-paper paper))
	 (cache (assoc url hotcrp-paper-cache))
	 (case-fold-search nil)
	 file)
    (cond
     ((eq 'inprogress (cdr cache))
      (message "Already fetching paper %s" paper))
     ((and cache (file-exists-p (cdr cache)))
      (hotcrp-view (cdr cache)))
     (t
      (if cache
	  (setcdr cache 'inprogress)
	(setq cache (cons url 'inprogress))
	(setq hotcrp-paper-cache (cons cache hotcrp-paper-cache)))
      (hotcrp-login url)
      (with-current-buffer (get-buffer-create " *hotcrp*")
	(kill-region (point-min) (point-max))
	(hotcrp-curl url)
	(goto-char (point-min))
	(if (not (re-search-forward "<a +href=['\"]\\([^'\"]*\\.pdf\\)['\"]"
				    nil t))
	    (error "Could not find URL of PDF"))
	(setq url (hotcrp-normalize-url url (match-string 1)))
	(setq file (make-temp-file (format "hotcrp-%s." paper) nil ".pdf"))
	(kill-region (point-min) (point-max))
	(let ((proc (hotcrp-curl-stdin 'async url nil "-o" file)))
	  (set-process-sentinel
	   proc `(lambda (p e) (hotcrp-view-sentinel ',cache ,file p e)))))))))
      

(defun hotcrp-number-index ()
  (let (res)
    (goto-char (point-min))
    (while (re-search-forward "^==\\+== Paper #\\([0-9]+\\)" nil t)
      (push (cons (match-string-no-properties 1) (match-beginning 0)) res))
    (nreverse res)))

(defun hotcrp-title-index ()
  (let (res)
    (goto-char (point-min))
    (while (re-search-forward
	    "^==\\+== Paper #\\([0-9]+\\).*\n==-== Title:\\s *\\(.*\\)\n"
	    nil t)
      (let ((pos (match-beginning 0))
	    (title (format "%3s: %s" (match-string-no-properties 1)
			   (match-string-no-properties 2))))
	(while (looking-at "^==-== \\s *\\(\\S .*\\)")
	  (setq title (concat title " " (match-string-no-properties 1)))
	  (forward-line))
	(push (cons title pos) res)))
    (nreverse res)))

(defun hotcrp-goto (&optional pref)
  "Go to the portion of a HotCRP review form for a particular paper."
  (interactive "P")
  (let* ((index (save-excursion (hotcrp-number-index)))
	 (paper (if pref
		    (format "%s" pref)
		  (completing-read "Paper number: " index
				   nil t nil 'hotcrp-num-hist)))
	 (pn (assoc paper index)))
    (if pn
	(goto-char (cdr pn))
      (error "No paper '%s'" paper))))

(defconst hotcrp-menu-re
  "==-== \\(\\(Choices: \\)?\\s *\\([0-9]+\\. \\)\\|\\s +\\)\\(.*\\)")
(defun hotcrp-extract-menu ()
  (save-excursion
    (let ((case-fold-search nil)
	  (cur "")
	  (options '((no-choice "*No Choice*" lambda () (interactive) nil
				"(Your choice here)")))
	  done)
      (beginning-of-line)
      (when (and (not (looking-at "^==[-+]== "))
		 (re-search-backward "^==[-+]== " nil t)
		 (looking-at "==-== Enter the number of your choice:"))
	(while (and (not done)
		    (eq 0 (forward-line -1))
		    (looking-at hotcrp-menu-re))
	  (let* ((number (match-string-no-properties 3))
		 (text (concat number (match-string-no-properties 4))))
	    (if (match-beginning 2) (setq done t))
	    (if (not number)
		(setq cur (concat " " text cur))
	      (setq text (concat text cur))
	      (setq cur "")
	      (push (list (intern (format "item-%s" number)) text
			  'lambda nil '(interactive) text)
		    options))))
	(and done
	     (nconc (list 'keymap "Options"
			  '(nop "*No Change*" lambda () (interactive) nil))
		    options))))))

(defun hotcrp-click (ev)
  (interactive "e")
  (let ((menu (hotcrp-extract-menu))
	item)
    (when (and menu (setq item (popup-menu menu)))
      (let ((b (save-excursion
		 (re-search-backward "^==[-+]== ")
		 (forward-line)
		 (point)))
	    (e (save-excursion
		 (if (not (re-search-forward "^==[-+]== " nil t))
		     (point-max)
		   (end-of-line 0)
		   (point)))))
	(kill-region b e)
	(insert (format "\n%s\n" item))
	))))

(defun hotcrp-extract-url ()
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "==\\+== .*Review Form")
      (error "Buffer does not start with paper review form"))
    (forward-line)
    (let (last)
      (while (looking-at "^==-==\\s *\\(.*\\)\\s *$")
	(setq last (match-string-no-properties 1))
	(forward-line))
      (unless (string-match "\\`http.*://.*offline.*\\'" last)
	(error "Could not extract URL from header"))
      last)))

(defun hotcrp-base-url ()
  (condition-case nil
      (save-match-data
	(hotcrp-normalize-url (hotcrp-extract-url) ""))
    (error nil)))

(defun hotcrp-url-of-paper (paper &optional base-url)
  (unless base-url
    (setq base-url (hotcrp-base-url)))
  (format "%spaper?p=%s" base-url paper))

(defun hotcrp-upload ()
  "Uploads a filled-out HotCRP review-form to the server.

Requires the offline-review URL to be stored in a comment at the
top of the file (which HotCRP does by default in review forms)."
  (interactive)
  (let ((url (hotcrp-extract-url))
	(in (current-buffer)))
    (when (and (buffer-modified-p)
	       (y-or-n-p (format "Save file %s? " (buffer-file-name))))
      (save-buffer))
    (when (y-or-n-p (format "Upload reviews to %s? " url))
      (hotcrp-login url)
      (with-current-buffer (get-buffer-create " *hotcrp*")
	(kill-region (point-min) (point-max))
	(hotcrp-curl-stdin in (concat url "?uploadForm=1&post=1") nil
			   "-F" "postnonempty=1" "-F" "uploadedFile=@-")))))

;;;###autoload
(define-derived-mode hotcrp-mode text-mode "HotCRP"
  "Major mode for editing HotCRP offline review forms.
\\<global-map>
To enter hotcrp-mode, either visit a file containing a review
form, or fetch one directly from the server by typing
\\[hotcrp-fetch] in an empty buffer.
\\<hotcrp-paper-map>
Once in hotcrp-mode, you can type \\[hotcrp-upload] to upload
your completed reviews to the server.

In a buffer with review forms for multiple papers, you can move
the point to the review for a particular paper with
\\[hotcrp-goto] or \\[imenu].

To view the PDF of a paper, type \\[hotcrp-fetch-pdf].  You can
also load the HotCRP review page for a particular paper in your
browser with \\[hotcrp-browse-paper-url].

When the mouse is hovering over the paper number at the start of
each review, you can additionally use \\[hotcrp-paper-click-reviews] to
navigate the the web page containing everybody else's reviews, or
\\[hotcrp-paper-click-paper] to download and view the PDF of the paper.

\\{hotcrp-mode-map}"

  (setq imenu-create-index-function 'hotcrp-title-index)

  (setq paragraph-separate
	(concat "^==.== \\|" paragraph-separate))

  (setq font-lock-defaults '(hotcrp-font-lock-keywords t))

  (make-local-variable 'font-lock-extra-managed-props)
  (make-local-variable 'char-property-alias-alist)
  (mapc (lambda (sym)
	  (let ((hcsym (intern (concat "hotcrp-" (symbol-name sym)))))
	    (add-to-list 'font-lock-extra-managed-props hcsym)
	    (add-to-list 'char-property-alias-alist (list sym hcsym))))
	'(read-only local-map mouse-face help-echo display))
  (add-to-list 'font-lock-extra-managed-props 'hotcrp-paper)
  (when (fboundp 'easy-menu-add)
    (easy-menu-add hotcrp-menu hotcrp-mode-map))
  )

(define-key hotcrp-mode-map "\C-c\C-g" 'hotcrp-goto)
(define-key hotcrp-mode-map "\C-cg" 'hotcrp-goto)
(define-key hotcrp-mode-map "\C-cp" 'hotcrp-fetch-pdf)
(define-key hotcrp-mode-map "\C-cu" 'hotcrp-upload)
(define-key hotcrp-mode-map "\C-cr" 'hotcrp-browse-paper-url)
(define-key hotcrp-mode-map [mouse-1] 'hotcrp-click)
(define-key hotcrp-mode-map [S-down-mouse-2] 'imenu)

(easy-menu-define
  hotcrp-menu hotcrp-mode-map "HotCRP menu"
  '("HotCRP"
    ["Fetch Review Form" hotcrp-fetch t]
    ["Fetch Paper" hotcrp-fetch-pdf t]
    ["Browse Paper URL" hotcrp-browse-paper-url t]
    ["Goto Review" imenu t]
    ["Upload Reviews" hotcrp-upload]))

(defun hotcrp-paper-of-click (event)
  (let* ((ev (event-end event))
	 (w (posn-window ev))
	 (b (and w (window-buffer w)))
	 (p (and b (posn-point ev))))
    (and p (get-text-property p 'hotcrp-paper b))))
(defun hotcrp-paper-click-reviews (event)
  "Invoke web browser on paper review web page."
  (interactive "e")
  (let* ((paper (hotcrp-paper-of-click event))
	 (url (and paper (hotcrp-url-of-paper paper))))
    (and url (browse-url url))))
(defun hotcrp-paper-click-paper (event)
  "Download and invoke web browser on a paper's PDF file."
  (interactive "e")
  (let* ((paper (hotcrp-paper-of-click event)))
    (and paper (hotcrp-fetch-pdf paper))))

(defconst hotcrp-paper-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map hotcrp-mode-map)
    (define-key map [mouse-2] 'hotcrp-paper-click-paper)
    (define-key map [mouse-1] 'hotcrp-paper-click-reviews)
    map))

(and (boundp 'magic-mode-alist)
     (add-to-list 'magic-mode-alist
		  '("\\`==\\+== .* Review Form" . hotcrp-mode)))

(provide 'hotcrp-mode)

;;; hotcrp-mode.el ends here
