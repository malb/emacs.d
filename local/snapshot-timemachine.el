;;; snapshot-timemachine.el --- Step through (Btrfs, ZFS, ...) snapshots of files

;; Copyright (C) 2015 by Thomas Winant

;; Author: Thomas Winant <dewinant@gmail.com>
;; URL: https://github.com/mrBliss/snapshot-timemachine
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Created: Apr 4 2015

;; This program is free software; you can redistribute it and/or modify
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

;; Snapshot-timemachine provides a polished interface to step through the
;; snapshots of a file made by a third-party snapshot or backup facility,
;; e.g. Btrfs, ZFS, etc.

;; Out of the box, it can detect snapshots made by Snapper, but it provides
;; a simple interface to add support for other snapshot facilities.

;; It provides two views: the timemachine (`snapper-timemachine') and the
;; timeline (`snapper-timeline').

;;; Code:

;; A quick overview of the design of this program:
;;
;; The program provides two main features: the `snapshot-timemachine' and the
;; `snapshot-timeline'. These names also function as namespaces for related
;; functions. As this file is called snapshot-timemachine, the
;; `snapshot-timemachine' namespace is used for common functionality.

;; What happens first when either of these functions is invoked is that the
;; snapshots of the visited file are retrieved using the function stored in
;; `snapshot-timemachine-snapshot-finder'. By default it is set to
;; `snapshot-timemachine-snapper-snapshot-finder', a function that will find
;; snapshots of a file made by Snapper (http://snapper.io).

;; You can use snapshot-timemachine for your own (custom) snapshot system
;; other than Snapper by defining a function to find snapshots. See the
;; docstring of `snapshot-timemachine-snapshot-finder' for more details.

;; After using the snapshot finding function, a new buffer is created and the
;; snapshots are stored in a buffer-local variable
;; `snapshot-timemachine--snapshots'. The original filename is stored in
;; `snapshot-timemachine--file'.

;; `snapshot-timemachine' stores the snapshots in
;; `snapshot-timemachine--snapshots' as a `snapshot-timemachine--zipper'. A
;; zipper is a data structure to walk through a list while maintaining an
;; element in focus. Going to the previous or next element is an O(1)
;; operation as opposed to a singly-linked list's O(n) (previous) and O(1)
;; (next). The trick is a keep track of triple: the focused element, a list of
;; the next elements, and a list of the previous elements. The first element
;; of this last list is the previous element, and its last element is the very
;; first element of the list as a whole. Shifting to the previous or next
;; element is a simply matter of executing `car', `cons', and `cdr' once each,
;; quite elegant!

;; `snapshot-timeline' stores snapshots as a regular list as it derives from
;; `tabulated-list-mode' which requires entries to be stored as a list
;; anyway. The snapshot belonging to a line is retrieved by finding the
;; snapshot in the list whose `snapshot-id' matches `tabulated-list-get-id'.

;; When going from one view to the other, the snapshots are reused and
;; converted to or from a zipper/list. When going from the timeline to the
;; timemachine, the buffer is recreated as its contents differ
;; anyway. However, going from the timemachine to the timeline, an existing
;; timeline buffer is reused.

;; Navigation through snapshots in the snapshot-timemachine happens by
;; modifying the zipper in `snapshot-timemachine--snapshots' and calling
;; `snapshot-timemachine-show-focused-snapshot' to let the buffer reflect the
;; updated zipper.

;; TODO
;; * sync diffs with timeline/timemachine as well?
;; * relative timestamps
;; * dired?
;; * compatibility with ZFS (http://wiki.complete.org/ZFSAutoSnapshots) and
;;   other common snapshot systems.


;; Imports

(require 'cl-lib)    ;; for cl-*
(require 'subr-x)    ;; for string-remove-prefix
(require 'diff-mode) ;; for the diff-{added,removed} faces
(require 'hl-line)   ;; for hl-line-{mode,highlight}

(defgroup snapshot-timemachine nil
  "Step through (Btrfs, ZFS, ...) snapshots of files"
  :group 'backup)

;; Customisation

(defvar snapshot-timemachine-time-format "%a %d %b %Y %R"
  "The format to use when displaying a snapshot's time.
The default format is \"sat 14 mar 2015 10:35\".")

(defvar snapshot-timemachine-diff-switches "-u"
  "The switches to pass to diff when comparing snapshots of a file.
See variable `diff-switches'.")

(defvar snapshot-timemachine-include-current t
  "Include the current version of the file in the list of snapshots.")

(defvar snapshot-timemachine-sync-with-timeline t
  "Keep the timemachine in sync with the timeline.
When going to a snapshot in the timeline, also go to it in the
timemachine and vice versa.  If, for some reason, loading a
snapshot takes a while (e.g. remote storage), setting this to nil
will make moving around in the timeline more responsive.")

;; Zipper

(cl-defstruct snapshot-timemachine--zipper
  "A zipper suited for tracking focus in a list.
Zippers must always contain at least one element, the focused element.

Slots:

`focus' The focused element.

`before' A list of the elements coming before the focused
         element.  The first element of the list is the element
         just before the focused element.  The last element of
         this list is the first element of the whole list
         represented by the zipper.

`after' A list of the elements coming after the focused element."
  focus before after)

(defun snapshot-timemachine--zipper-from-list (l)
  "Make a zipper from the given list L.
The first element of the list will be focused.  Return nil when
the list was empty."
  (when l
    (make-snapshot-timemachine--zipper
     :focus  (car l)
     :before nil
     :after  (cdr l))))

(defun snapshot-timemachine--zipper-to-list (z)
  "Convert the zipper Z back to a list.
The order is preserved, but the focus is lost."
  (let ((l (cons (snapshot-timemachine--zipper-focus z)
                 (snapshot-timemachine--zipper-after z)))
        (before (snapshot-timemachine--zipper-before z)))
    (while before
      (push (car before) l)
      (setq before (cdr before)))
    l))

(defun snapshot-timemachine--zipper-at-end (z)
  "Return t when the zipper Z is at the last element of the list."
  (null (snapshot-timemachine--zipper-after z)))

(defun snapshot-timemachine--zipper-at-start (z)
  "Return t when the zipper Z is at the first element of the list."
  (null (snapshot-timemachine--zipper-before z)))

(defun snapshot-timemachine--zipper-shift-next (z)
  "Shift the zipper Z to the next element in the list.
Return Z unchanged when at the last element."
  (if (snapshot-timemachine--zipper-at-end z) z
    (make-snapshot-timemachine--zipper
     :focus  (car (snapshot-timemachine--zipper-after z))
     :before (cons (snapshot-timemachine--zipper-focus z)
                   (snapshot-timemachine--zipper-before z))
     :after  (cdr (snapshot-timemachine--zipper-after z)))))

(defun snapshot-timemachine--zipper-shift-prev (z)
  "Shift the zipper Z to the previous element in the list.
Return Z unchanged when at the first element."
  (if (snapshot-timemachine--zipper-at-start z) z
    (make-snapshot-timemachine--zipper
     :focus  (car (snapshot-timemachine--zipper-before z))
     :before (cdr (snapshot-timemachine--zipper-before z))
     :after  (cons (snapshot-timemachine--zipper-focus z)
                   (snapshot-timemachine--zipper-after z)))))

(defun snapshot-timemachine--zipper-shift-end (z)
  "Shift the zipper Z to the last element in the list.
Return Z unchanged when already at the last element in the list."
  (if (snapshot-timemachine--zipper-at-end z) z
    (let ((new-before (cons (snapshot-timemachine--zipper-focus z)
                            (snapshot-timemachine--zipper-before z)))
          (after (snapshot-timemachine--zipper-after z)))
      (while (cdr after)
        (push (car after) new-before)
        (setq after (cdr after)))
      (make-snapshot-timemachine--zipper
       :focus (car after)
       :before new-before
       :after nil))))

(defun snapshot-timemachine--zipper-shift-start (z)
  "Shift the zipper Z to the first element in the list.
Return Z unchanged when already at the first element in the
list."
  (if (snapshot-timemachine--zipper-at-start z) z
    (let ((new-after (cons (snapshot-timemachine--zipper-focus z)
                           (snapshot-timemachine--zipper-after z)))
          (before (snapshot-timemachine--zipper-before z)))
      (while (cdr before)
        (push (car before) new-after)
        (setq before (cdr before)))
      (make-snapshot-timemachine--zipper
       :focus (car before)
       :before nil
       :after new-after))))

(defun snapshot-timemachine--zipper-shift-forwards-to (z predicate)
  "Shift the zipper Z forwards to an element satisfying PREDICATE.
Return nil when no element satisfies PREDICATE or when Z is nil."
  (when z
    (cl-loop for z* = z then (snapshot-timemachine--zipper-shift-next z*)
             if (funcall predicate (snapshot-timemachine--zipper-focus z*))
             return z*
             until (snapshot-timemachine--zipper-at-end z*))))

(defun snapshot-timemachine--zipper-shift-backwards-to (z predicate)
  "Shift the zipper Z backwards to an element satisfying PREDICATE.
Returns nil when no element satisfies PREDICATE or when Z is
nil."
  (when z
    (cl-loop for z* = z then (snapshot-timemachine--zipper-shift-prev z*)
             if (funcall predicate (snapshot-timemachine--zipper-focus z*))
             return z*
             until (snapshot-timemachine--zipper-at-start z*))))

(defun snapshot-timemachine--zipper-shift-to (z predicate)
  "Shift the zipper Z to an element satisfying PREDICATE.
First try the next elements, then the previous ones.  Return nil
when no element satisfies PREDICATE or when Z is nil."
  (or
   (snapshot-timemachine--zipper-shift-forwards-to  z predicate)
   (snapshot-timemachine--zipper-shift-backwards-to z predicate)))

;; Internal variables

(defvar-local snapshot-timemachine--snapshots nil
  "A data structure storing the `snapshot' structs.
Will be a zipper in `snapshot-timemachine' buffers.
In `snapshot-timeline' buffers it will be a list.")

(defvar-local snapshot-timemachine--file nil
  "Maintains the path to the original (most recent) file.")

;; Snapshot struct and helpers

(cl-defstruct snapshot
  "A struct representing a snapshot.

Slots:

`id' An ascending numerical identifier for internal lookups.

`name' The name of the snapshot that will be displayed in the
       timemachine and the timeline.

`file' The absolute path to the snapshotted file,
       e.g. \"/home/.snapshots/2/snapshot/thomas/.emacs.d/init.el\".

`date' The date/time at which the snapshot was made,
       format: (HIGH LOW USEC PSEC), a standard Emacs time
       object.

`diffstat' The number of lines added/removed compared to the
           previous snapshot, format: (ADDED . REMOVED). Can be
           nil when uninitialised."
  id name file date diffstat)

(defun snapshot-timemachine-interesting-diffstatp (diffstat)
  "Return t when the given DIFFSTAT (format: (ADDED . REMOVED)) is interesting.
A diffstat is interesting when it is not nil and ADDED or REMOVED
is greater than zero."
  (and diffstat
       (or (< 0 (car diffstat))
           (< 0 (cdr diffstat)))))

(defun snapshot-interestingp (s)
  "Return t when snapshot S's diffstat is interesting.
See `snapshot-timemachine-interesting-diffstatp' to know what
'interesting' means in this context."
  (snapshot-timemachine-interesting-diffstatp (snapshot-diffstat s)))

;; Locating snapshots

(defun snapshot-timemachine-find-dir (file &optional dir)
  "Look for FILE by climbing up the directory tree starting from DIR.
FILE can be a directory or a file.  DIR defaults to
`default-directory'.  Return nil when the FILE is not found.
Stops at \"/\".  Note: why not use `locate-dominating-file'?
Because it stops at \"~\"."
  (let* ((dir (or dir default-directory))
         (file-in-dir (expand-file-name file dir)))
    (if (file-exists-p file-in-dir)
        file-in-dir
      (let ((parent-dir (file-name-directory (directory-file-name dir))))
        (unless (equal "/" parent-dir)
          (snapshot-timemachine-find-dir file parent-dir))))))

(defun snapshot-timemachine-snapper-snapshot-finder (file)
  "Find snapshots of FILE made by Snapper.
Look for an ancestor directory containing a folder called
\".snapshots\", which contains numbered snapshot folders.  Each
snapshot folder has a subfolder called \"snapshot\" containing
the actual snapshotted subtree.

For example, say FILE is
\"/home/thomas/.emacs.d/init.el\"

And the snapshots are stored in \"/home/.snapshots/\", the
snapshots of the file will be:
\"/home/.snapshots/2/thomas/.emacs.d/init.el\",
\"/home/.snapshots/10/thomas/.emacs.d/init.el\", ...,
\"/home/.snapshots/100/thomas/.emacs.d/init.el\""
  (let* ((file (expand-file-name file)) ;; "/home/thomas/.emacs.d/init.el"
         (snapshot-dir
          (snapshot-timemachine-find-dir
           ".snapshots" (directory-file-name file)))) ;; "/home/.snapshots"
    (if (null snapshot-dir)
        (error "Could not find a .snapshots directory")
      (let* ((common-prefix (file-name-directory snapshot-dir)) ;; "/home/"
             ;; "thomas/.emacs.d/init.el"
             (rel-path (string-remove-prefix common-prefix file)))
        (cl-loop for sdir in (directory-files snapshot-dir t)
                 for filename = (file-name-nondirectory sdir) ;; "2"
                 for abs-path = (format "%s/snapshot/%s" sdir rel-path)
                 ;; "/home/.snapshots/2/thomas/.emacs.d/init.el"
                 when (and (string-match-p "[0-9]+" filename)
                           (file-exists-p abs-path))
                 collect (make-snapshot
                          :id (string-to-number filename)
                          :name filename
                          :file abs-path
                          :date (nth 5 (file-attributes sdir))))))))

(defcustom snapshot-timemachine-snapshot-finder
  #'snapshot-timemachine-snapper-snapshot-finder
  "The function used to retrieve the snapshots for a given file.
The function should accept an absolute path to a file and return
a list of `snapshot' structs of existing snapshots of the file.
The `diffstat' can still remain nil, and will be filled in later."
  :type 'function)

(defun snapshot-timemachine-diffstat (file1 file2)
  "Calculate a diffstat between FILE1 and FILE2.
The result is cons cell (ADDED . REMOVED) of the number of lines
added and the number of lines removed going from FILE1 to FILE2.
Return nil when one of the two files is missing (or nil)."
  (when (and file1 file2 (file-exists-p file1) (file-exists-p file2))
    (let ((diff-output
           (shell-command-to-string
            (format "diff %s %s %s \"%s\" \"%s\""
                    "--old-line-format='-'"
                    "--new-line-format='+'"
                    "--unchanged-line-format=''"
                    file1 file2))))
      (cl-loop for c across diff-output
               count (eq c ?+) into p
               count (eq c ?-) into m
               finally return (cons p m)))))

(defun snapshot-timemachine-find-snapshots (file)
  "Return a list of all the snapshots of FILE.
Call the function stored in
`snapshot-timemachine-snapshot-finder' for this purpose.  The
snapshots will be sorted from oldest to newest.  Includes the
current version of the file when
`snapshot-timemachine-include-current' is non-nil.  The snapshot
representing the current version will have `most-positive-fixnum'
as `id'."
  (let ((snapshots
         (cl-sort
          (funcall snapshot-timemachine-snapshot-finder file)
          #'time-less-p :key #'snapshot-date)))
    ;; Append (mutate) the current file when the option is set
    (when snapshot-timemachine-include-current
      (let ((current (make-snapshot
                      :id most-positive-fixnum
                      :name "current"
                      :file file
                      :date (nth 5 (file-attributes file)))))
        (nconc snapshots (list current))))
    ;; Fill in the diffstats (mutate)
    (cl-loop
     for s in snapshots and s-prev in (cons nil snapshots)
     when s-prev
     for diffstat = (snapshot-timemachine-diffstat
                     (snapshot-file s-prev)
                     (snapshot-file s))
     do (setf (snapshot-diffstat s) diffstat))
    ;; Return the (mutated) snapshots
    snapshots))

;; Interactive timemachine functions and their helpers

(defun snapshot-timemachine-show-focused-snapshot ()
  "Display the currently focused snapshot in the buffer.
The current snapshot is stored in
`snapshot-timemachine--snapshots'.  This function should be
called after changing the focused snapshot in
`snapshot-timemachine--snapshots' to update the buffer."
  (let* ((snapshot (snapshot-timemachine--zipper-focus
                    snapshot-timemachine--snapshots))
         (file (snapshot-file snapshot))
         (time (format-time-string
                snapshot-timemachine-time-format
                (snapshot-date snapshot))))
    (setq buffer-read-only nil)
    (insert-file-contents file nil nil nil t)
    (setq buffer-read-only t
          buffer-file-name file
          default-directory (file-name-directory file)
          mode-line-buffer-identification
          (list (propertized-buffer-identification "%12b") "@"
                (propertize
                 (snapshot-name snapshot)
                 'face 'bold)
                " " time))
    (set-buffer-modified-p nil)
    (message "Snapshot %s from %s"
             (snapshot-name snapshot) time)))

(defun snapshot-timemachine-sync-timeline ()
  "Focus the same snapshot in the timeline as in the timemachine.
Only acts when `snapshot-timemachine-sync-with-timeline' is
non-nil, in which case the same snapshot is focused in the
corresponding timeline buffer as in the current timemachine
buffer.  Doesn't try to create a timeline buffer if there is
none."
  (when snapshot-timemachine-sync-with-timeline
    (let ((id (snapshot-id
               (snapshot-timemachine--zipper-focus
                snapshot-timemachine--snapshots)))
          (timeline (snapshot-timemachine-get-timeline-buffer)))
      (when timeline
        (with-current-buffer timeline
          (snapshot-timeline-goto-snapshot-with-id id))))))

(defun snapshot-timemachine-show-next-snapshot ()
  "Show the next snapshot."
  (interactive)
  (if (snapshot-timemachine--zipper-at-end snapshot-timemachine--snapshots)
      (message "Last snapshot")
    (setq snapshot-timemachine--snapshots
          (snapshot-timemachine--zipper-shift-next
           snapshot-timemachine--snapshots))
    (snapshot-timemachine-show-focused-snapshot)
    (snapshot-timemachine-sync-timeline)))

(defun snapshot-timemachine-show-prev-snapshot ()
  "Show the previous snapshot."
  (interactive)
  (if (snapshot-timemachine--zipper-at-start snapshot-timemachine--snapshots)
      (message "First snapshot")
    (setq snapshot-timemachine--snapshots
          (snapshot-timemachine--zipper-shift-prev
           snapshot-timemachine--snapshots))
    (snapshot-timemachine-show-focused-snapshot)
    (snapshot-timemachine-sync-timeline)))

(defun snapshot-timemachine-show-first-snapshot ()
  "Show the first snapshot."
  (interactive)
  (if (snapshot-timemachine--zipper-at-start snapshot-timemachine--snapshots)
      (message "Already at first snapshot")
    (setq snapshot-timemachine--snapshots
          (snapshot-timemachine--zipper-shift-start
           snapshot-timemachine--snapshots))
    (snapshot-timemachine-show-focused-snapshot)
    (snapshot-timemachine-sync-timeline)))

(defun snapshot-timemachine-show-last-snapshot ()
  "Show the last snapshot."
  (interactive)
  (if (snapshot-timemachine--zipper-at-end snapshot-timemachine--snapshots)
      (message "Already at last snapshot")
    (setq snapshot-timemachine--snapshots
          (snapshot-timemachine--zipper-shift-end
           snapshot-timemachine--snapshots))
    (snapshot-timemachine-show-focused-snapshot)
    (snapshot-timemachine-sync-timeline)))

(defun snapshot-timemachine-goto-snapshot-with-id (id)
  "Show the snapshot with the given ID.
Must be called from within a snapshot-timemachine buffer.  Throws
an error when there is no such snapshot."
  (unless (= id (snapshot-id
                 (snapshot-timemachine--zipper-focus
                  snapshot-timemachine--snapshots)))
    (let ((z (snapshot-timemachine--zipper-shift-to
              snapshot-timemachine--snapshots
              (lambda (s)
                (= (snapshot-id s) id)))))
      (if (null z)
          (error "No snapshot with ID: %d" id)
        (setq snapshot-timemachine--snapshots z)
        (snapshot-timemachine-show-focused-snapshot)
        (snapshot-timemachine-sync-timeline)))))

(defun snapshot-timemachine-show-nth-snapshot ()
  "Interactively choose which snapshot to show."
  (interactive)
  (let* ((candidates
          (mapcar (lambda (snapshot)
                    (cons
                     (format "Snapshot %s from %s"
                             (snapshot-name snapshot)
                             (format-time-string
                              snapshot-timemachine-time-format
                              (snapshot-date snapshot)))
                     (snapshot-id snapshot)))
                  (snapshot-timemachine--zipper-to-list
                   snapshot-timemachine--snapshots)))
         (id (cdr (assoc
                   (completing-read
                    "Choose snapshot: " candidates nil t)
                   candidates))))
    (when id
      (snapshot-timemachine-goto-snapshot-with-id id))))

(defun snapshot-timemachine-show-next-interesting-snapshot ()
  "Show the next snapshot that differs from the current one."
  (interactive)
  (if (snapshot-timemachine--zipper-at-end snapshot-timemachine--snapshots)
      (message "Last snapshot")
    (let ((z* (snapshot-timemachine--zipper-shift-forwards-to
               (snapshot-timemachine--zipper-shift-next
                snapshot-timemachine--snapshots)
               #'snapshot-interestingp)))
      (if (null z*)
          (message "No next differing snapshot found.")
        (setq snapshot-timemachine--snapshots z*)
        (snapshot-timemachine-show-focused-snapshot)
        (snapshot-timemachine-sync-timeline)))))

(defun snapshot-timemachine-show-prev-interesting-snapshot ()
  "Show the previous snapshot that differs from the current one."
  (interactive)
  (if (snapshot-timemachine--zipper-at-start snapshot-timemachine--snapshots)
      (message "First snapshot")
    (let ((z* (snapshot-timemachine--zipper-shift-backwards-to
               (snapshot-timemachine--zipper-shift-prev
                snapshot-timemachine--snapshots)
               #'snapshot-interestingp)))
      (if (null z*)
          (message "No previous differing snapshot found.")
        (setq snapshot-timemachine--snapshots z*)
        (snapshot-timemachine-show-focused-snapshot)
        (snapshot-timemachine-sync-timeline)))))

(defun snapshot-timemachine-get-timeline-buffer (&optional create-missing)
  "Get the corresponding timeline buffer.
The current buffer must be a timemachine buffer.  Return nil if
no existing buffer is found, unless CREATE-MISSING is non-nil, in
which case a new one is created and returned."
  (let* ((name (format
                "timeline:%s"
                (file-name-nondirectory snapshot-timemachine--file)))
         ;; A buffer with the correct name
         (correct-name (get-buffer name))
         (file snapshot-timemachine--file))
    ;; That also has the correct absolute path to the original file.  If we
    ;; didn't check this, we would get into trouble when the user opened
    ;; timelines of more than one file with the same name. TODO test this
    (cond ((and correct-name
                (with-current-buffer correct-name
                  (equal file snapshot-timemachine--file)))
           correct-name)
          (create-missing
           (snapshot-timeline-create
            snapshot-timemachine--file
            (snapshot-timemachine--zipper-to-list
             snapshot-timemachine--snapshots)))
          ;; Better to be explicit: when no buffer was found and
          ;; CREATE-MISSING was nil, return nil.
          (t nil))))

(defun snapshot-timemachine-show-timeline ()
  "Display the snapshot timeline of the given file.
Leave the point on the line in the timeline of the snapshot that
was active in the timemachine."
  (interactive)
  (let ((focused-snapshot-id
         (snapshot-id (snapshot-timemachine--zipper-focus
                       snapshot-timemachine--snapshots))))
    (with-current-buffer
        (switch-to-buffer (snapshot-timemachine-get-timeline-buffer t))
      ;; Go to the snapshot that was active in the timemachine
      (snapshot-timeline-goto-snapshot-with-id focused-snapshot-id))))

(defun snapshot-timemachine-restore-snapshot (snapshot)
  "Restore the given SNAPSHOT.
Replace the current file with the snapshotted file's contents.
Prompt the user for confirmation.  Use the buffer-local variable
`snapshot-timemachine--file' to find out the location of the
current file.  Do nothing when SNAPSHOT is nil."
  (when (and snapshot
             (y-or-n-p
              (format "Replace the contents of %s with snapshot %s? "
                      (file-name-nondirectory snapshot-timemachine--file)
                      (snapshot-name snapshot))))
    (with-current-buffer (find-file snapshot-timemachine--file)
      ;; We do want the undo
      (erase-buffer)
      (insert-file-contents (snapshot-file snapshot))
      (set-buffer-modified-p t))))

(defun snapshot-timemachine-restore ()
  "Restore the focused snapshot.
Replace the current file with the snapshotted file's contents.
Prompt the user for confirmation."
  (interactive)
  (snapshot-timemachine-restore-snapshot
   (snapshot-timemachine--zipper-focus snapshot-timemachine--snapshots)))

(defun snapshot-timemachine-quit ()
  "Exit the timemachine."
  (interactive)
  (kill-buffer))

;; Minor-mode for snapshots

(define-minor-mode snapshot-timemachine-mode
  "Step through snapshots of files."
  :init-value nil
  :lighter " Timemachine"
  :keymap
  '(("n" . snapshot-timemachine-show-next-snapshot)
    ("p" . snapshot-timemachine-show-prev-snapshot)
    ("N" . snapshot-timemachine-show-next-interesting-snapshot)
    ("P" . snapshot-timemachine-show-prev-interesting-snapshot)
    ("<" . snapshot-timemachine-show-first-snapshot)
    (">" . snapshot-timemachine-show-last-snapshot)
    ("j" . snapshot-timemachine-show-nth-snapshot)
    ("t" . snapshot-timemachine-show-timeline)
    ("l" . snapshot-timemachine-show-timeline)
    ("q" . snapshot-timemachine-quit)
    ("r" . snapshot-timemachine-restore)
    ("s" . write-file))
  :group 'snapshot-timemachine)

;; Timemachine launcher

(defun snapshot-timemachine-create (file snapshots)
  "Create and return a snapshot timemachine buffer.
The snapshot timemachine will be of FILE using SNAPSHOTS.
SNAPSHOTS must be a non-empty list.  The last snapshot is
displayed.  Return the created buffer."
  (let ((timemachine-buffer
         (format "snapshot:%s" (file-name-nondirectory file)))
        ;; We say it must be non-empty, so
        ;; `snapshot-timemachine--zipper-from-list' shouldn't fail.
        (z (snapshot-timemachine--zipper-from-list snapshots)))
    (cl-destructuring-bind (cur-line mode)
        (with-current-buffer (find-file-noselect file t)
          (list (line-number-at-pos) major-mode))
      (with-current-buffer (get-buffer-create timemachine-buffer)
        (funcall mode)
        (setq snapshot-timemachine--file file
              snapshot-timemachine--snapshots
              (snapshot-timemachine--zipper-shift-end z))
        (snapshot-timemachine-show-focused-snapshot)
        (goto-char (point-min))
        (forward-line (1- cur-line))
        (snapshot-timemachine-mode)
        (current-buffer)))))

;;;###autoload
(defun snapshot-timemachine (&optional file)
  "Start the snapshot timemachine for FILE.
FILE defaults to the file the current buffer is visiting."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (if (null file)
        (message "Buffer is not visiting a file")
      (let ((snapshots (snapshot-timemachine-find-snapshots file)))
        (if (null snapshots)
            (message "No snapshots found")
          (switch-to-buffer
           (snapshot-timemachine-create file snapshots)))))))

;; Interactive timeline functions and their helpers

(defun snapshot-timeline-format-diffstat (diffstat &optional width)
  "Format DIFFSTAT as plus and minus signs with a maximum width of WIDTH.
WIDTH defaults to 64 characters.  When the DIFFSTAT is nil or (0
. 0), an empty string is returned.  Otherwise, a string
consisting of a plus sign (with face `diff-added') for each added
line and a minus sign (with face `diff-removed') for each removed
line.  If the total number of signs would exceed WIDTH, the
number of plus and minus signs is relative to WIDTH."
  (cl-destructuring-bind (pluses . minuses) diffstat
    (let ((width (or width 64))
          (total (+ pluses minuses)))
      (when (> total width)
        (setq pluses (round (* width (/ pluses (float total))))
              minuses (- width pluses)))
      (concat (propertize (make-string pluses ?+)
                          'face 'diff-added)
              (propertize (make-string minuses ?-)
                          'face 'diff-removed)))))

(defun snapshot-timeline-format-snapshots (snapshots &optional interesting-only)
  "Format SNAPSHOTS to be used as `tabulated-list-entries'.
An entry consists of the snapshot's name, its date and a diffstat
with the previous snapshot.  If INTERESTING-ONLY is non-nil, only
snapshots in which the file was changed are returned."
  (cl-loop
   for s in snapshots
   for diffstat = (snapshot-diffstat s)
   unless (and interesting-only (not (snapshot-interestingp s)))
   collect (list (snapshot-id s)
                 (vector
                  (format "%5s" ;; TODO configurable
                          ;; We do it like this because we don't want the padding
                          ;; spaces to be underlined
                          (propertize (snapshot-name s)
                                      'face 'button))
                  (list (format-time-string
                         snapshot-timemachine-time-format
                         (snapshot-date s))
                        :time (snapshot-date s))
                  (if diffstat
                      (snapshot-timeline-format-diffstat diffstat 40)
                    "")))))

(defun snapshot-timeline-all-displayedp ()
  "Return t when all snapshots are displayed, not only 'interesting' ones.
Otherwise return nil."
  ;; When there are as many entries displayed as there are snapshots, we
  ;; assume we're displaying all entries.  The condition can also be true when
  ;; all snapshots are interesting, in which case all snapshots are displayed
  ;; anyway.
  (= (length tabulated-list-entries)
     (length snapshot-timemachine--snapshots)))

(defun snapshot-timeline-toggle-interesting-only ()
  "Toggle between showing all and only interesting snapshots.
A snapshot is interesting when it differs from the previous
snapshot."
  (interactive)
  (setq tabulated-list-entries
        (snapshot-timeline-format-snapshots
         snapshot-timemachine--snapshots
         (snapshot-timeline-all-displayedp)))
  (tabulated-list-print t))

(defun snapshot-timeline-show-snapshot-or-diff ()
  "Show the snapshot or diff under the point, depending on the column.
If the point is located in the Diffstat column, a diff with the
previous snapshot is shown (`snapshot-timeline-show-diff'),
otherwise the snapshot of the file is
shown (`snapshot-timeline-show-snapshot-or-diff')."
  (interactive)
  (if (equal "Diffstat"
             (get-text-property (point) 'tabulated-list-column-name))
      (snapshot-timeline-show-diff)
    (snapshot-timeline-show-snapshot)))

(defun snapshot-timeline-get-timemachine-buffer (&optional create-missing)
  "Get the corresponding timemachine buffer.
The current buffer must be a timeline buffer.  Return nil if no
existing buffer is found, unless CREATE-MISSING is non-nil, in
which case a new one is created and returned."
  (let* ((name (format
                "snapshot:%s"
                (file-name-nondirectory snapshot-timemachine--file)))
         ;; A buffer with the correct name
         (correct-name (get-buffer name))
         (file snapshot-timemachine--file))
    ;; That also has the correct absolute path to the original file.  If we
    ;; didn't check this, we would get into trouble when the user opened
    ;; timelines of more than one file with the same name. TODO test this
    (cond ((and correct-name
                (with-current-buffer correct-name
                  (equal file snapshot-timemachine--file)))
           correct-name)
          (create-missing (snapshot-timemachine-create
                           snapshot-timemachine--file
                           snapshot-timemachine--snapshots))
          ;; Better to be explicit: when no buffer was found and CREATE was
          ;; nil, return nil.
          (t nil))))

(defun snapshot-timeline-show-snapshot ()
  "Show the snapshot under the point in the snapshot timemachine.
Open the timemachine buffer in the same window."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (null id)
        (message "Not on a snapshot")
      (with-current-buffer
          (switch-to-buffer
           (snapshot-timeline-get-timemachine-buffer t))
        (snapshot-timemachine-goto-snapshot-with-id id)))))

(defun snapshot-timeline-view-snapshot ()
  "Show the snapshot under the point in the snapshot timemachine.
Open the timemachine buffer in another window and leave the
timeline window focused."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (null id)
        (message "Not on a snapshot"))
    (let ((buf (snapshot-timeline-get-timemachine-buffer t)))
      (with-current-buffer buf
        (snapshot-timemachine-goto-snapshot-with-id id))
      (display-buffer buf))))

(defun snapshot-timeline-show-diff ()
  "Show the diff between this snapshot and the previous one.
When there is no previous snapshot, a message will tell the user
so."
  (interactive)
  (let* ((id1 (save-excursion (forward-line -1) (tabulated-list-get-id)))
         (id2 (tabulated-list-get-id))
         (s1 (snapshot-timeline-snapshot-by-id id1))
         (s2 (snapshot-timeline-snapshot-by-id id2)))
    (if (or (null s1) (null s2) (= (snapshot-id s1) (snapshot-id s2)))
        (message "No diff here")
      (snapshot-timeline-show-diff-between s1 s2))))

(defun snapshot-timeline-snapshot-by-id (id)
  "Return the snapshot in `snapshot-timemachine--snapshots' with ID.
Return nil when no snapshot matches the ID."
  (car (cl-member id snapshot-timemachine--snapshots
                  :key #'snapshot-id)))

(defun snapshot-timeline-get-A-and-B ()
  "Return a cons cell of the ids of the marked snapshots.
Format: (A . B) where A is an int or nil when it's not set, idem
for B."
  (let (a b)
    (save-excursion
      (cl-loop for pos = (progn (goto-char (point-min)) (point))
               then (progn (forward-line) (point))
               while (< pos (point-max))
               if (eq ?A (char-after pos))
               do (setq a (tabulated-list-get-id))
               if (eq ?B (char-after pos))
               do (setq b (tabulated-list-get-id))
               finally return (cons a b)))))

(defun snapshot-timeline-show-diff-between (s1 s2)
  "Show the diff between snapshots S1 and S2.
Open a buffer using `diff'."
  (diff (snapshot-file s1) (snapshot-file s2)
        snapshot-timemachine-diff-switches)
  (let* ((diff-buffer (get-buffer "*Diff*"))
         (new-name (format "diff:%s" (file-name-nondirectory
                                      snapshot-timemachine--file)))
         (existing-buffer (get-buffer new-name)))
    (when diff-buffer
      (when existing-buffer
        (kill-buffer existing-buffer))
      (with-current-buffer diff-buffer
        (rename-buffer new-name)
        (setq mode-line-buffer-identification
              (list (propertized-buffer-identification "%12b") "@"
                    (propertize
                     (format "%s..%s" (snapshot-name s1) (snapshot-name s2))
                     'face 'bold)))))))

(defun snapshot-timeline-validate-A-B (fn)
  "Check that A and B are marked, then call FN with the corresponding snapshots.
The user is informed of missing marks.  FN must accept two
arguments, the snapshots on which the A and B marks are placed."
  (cl-destructuring-bind (a . b) (snapshot-timeline-get-A-and-B)
    (if (or (null a) (null b))
        (message "Please mark both A and B.")
      (funcall fn
               (snapshot-timeline-snapshot-by-id a)
               (snapshot-timeline-snapshot-by-id b)))))

(defmacro with-A-B (args &rest body)
  "Call `snapshot-timeline-validate-A-B' passing a lambda with ARGS and BODY.
ARGS should be a list of two arguments, snapshots indicated by
marks A and B will be bound to them."
  (declare (indent 1))
  `(snapshot-timeline-validate-A-B (lambda ,args ,@body)))

(defun snapshot-timeline-show-diff-A-B ()
  "Show the `diff' between the snapshots marked as A and B.
The user is informed of missing marks."
  (interactive)
  (with-A-B (a b) (snapshot-timeline-show-diff-between a b)))

(defun snapshot-timeline-ediff-A-B ()
  "Start an `ediff' session between the snapshots marked as A and B.
The user is informed of missing marks."
  (interactive)
  (with-A-B (a b) (ediff (snapshot-file a) (snapshot-file b))))

(defun snapshot-timeline-emerge-A-B ()
  "Start an `emerge' session between the snapshots marked as A and B.
The user is informed of missing marks."
  (interactive)
  (with-A-B (a b) (emerge-files nil (snapshot-file a) (snapshot-file b) nil)))

(defun snapshot-timeline-mark-as-A ()
  "Mark a snapshot to use as file A of a diff."
  (interactive)
  (snapshot-timeline-unmark-all ?A)
  (tabulated-list-put-tag "A"))

(defun snapshot-timeline-mark-as-B ()
  "Mark a snapshot to use as file B of a diff."
  (interactive)
  (snapshot-timeline-unmark-all ?B)
  (tabulated-list-put-tag "B"))

(defun snapshot-timeline-unmark ()
  "Remove the mark on the current line."
  (interactive)
  (tabulated-list-put-tag ""))

(defun snapshot-timeline-unmark-all (&optional c)
  "Remove all marks (equal to C when passed) from the timeline.
When C is passed and non-nil, only marks matching C are removed,
otherwise all marks are removed."
  (interactive)
  (save-excursion
    (cl-loop for pos = (progn (goto-char (point-min)) (point))
             then (progn (forward-line) (point))
             while (< pos (point-max))
             if (or (null c) (eq c (char-after pos)))
             do (progn (goto-char pos)
                       (tabulated-list-put-tag "")))))

(defun snapshot-timeline-sync-timemachine ()
  "Show the same snapshot in the timemachine.
Only acts when `snapshot-timemachine-sync-with-timeline' is
non-nil, in which case the same snapshot is shown in the
corresponding timemachine buffer as in the current timeline
buffer.  Doesn't try to create a timemachine buffer if there is
none."
  (when snapshot-timemachine-sync-with-timeline
    (let ((id (tabulated-list-get-id))
          (timemachine (snapshot-timeline-get-timemachine-buffer)))
      (when timemachine
        (with-current-buffer timemachine
          (snapshot-timemachine-goto-snapshot-with-id id))))))

(defun snapshot-timeline-goto-snapshot-with-id (id)
  "Go to the snapshot with the given ID.
Must be called from within a snapshot-timeline buffer.  Throws an
error when there is no such snapshot."
  ;; No need to move when we're on the right snapshot
  (unless (= id (tabulated-list-get-id))
    ;; To actually move the point in a buffer displayed in another window, we
    ;; have to use `with-selected-window' instead of `with-current-buffer'. We
    ;; really want the point to move so that the selected snapshot in the
    ;; timeline is in sync with the snapshot displayed in the time
    ;; machine. Edge case: the timeline buffer is displayed in multiple
    ;; windows, so move the point in all these windows. We could optimise this
    ;; and reuse the position found in the first window, but this is not worth
    ;; the complexity.
    (dolist (w (get-buffer-window-list nil nil t))
      (with-selected-window w
        (cl-loop for pos = (progn (goto-char (point-min)) (point-min))
                 then (progn (forward-line) (point))
                 while (< pos (point-max))
                 until (= id (tabulated-list-get-id pos))))
      (hl-line-highlight)))
  ;; We didn't find the snapshot
  (when (= (point) (point-max))
    (if (snapshot-timeline-all-displayedp)
        (error "No snapshot with ID: %d" id)
      ;; If only the interesting ones were shown, try again with all entries
      (snapshot-timeline-toggle-interesting-only)
      (snapshot-timeline-goto-snapshot-with-id id))))

(defun snapshot-timeline-goto-start ()
  "Go to the first snapshot in the timeline.
The first snapshot in the timeline is not always chronologically
the first snapshot, for example when the order is reversed."
  (interactive)
  (goto-char (point-min))
  (snapshot-timeline-sync-timemachine))

(defun snapshot-timeline-goto-end ()
  "Go to the last snapshot in the timeline.
The last snapshot in the timeline is not always chronologically
the last snapshot, for example when the order is reversed."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (snapshot-timeline-sync-timemachine))

(defun snapshot-timeline-goto-next-snapshot ()
  "Go to the next snapshot in the timeline."
  (interactive)
  (forward-line)
  ;; Don't go beyond the timeline list
  (if (= (point) (point-max))
      (forward-line -1)
    (snapshot-timeline-sync-timemachine)))

(defun snapshot-timeline-goto-prev-snapshot ()
  "Go to the previous snapshot in the timeline."
  (interactive)
  (forward-line -1)
  (snapshot-timeline-sync-timemachine))

(defun snapshot-timeline-goto-next-interesting-snapshot ()
  "Go to the next snapshot that differs from the current."
  (interactive)
  (cl-loop for pos = (progn (forward-line) (point))
           while (< pos (point-max))
           for id = (tabulated-list-get-id)
           for s = (snapshot-timeline-snapshot-by-id id)
           until (and s (snapshot-interestingp s)))
  (snapshot-timeline-sync-timemachine))

(defun snapshot-timeline-goto-prev-interesting-snapshot ()
  "Go to the previous snapshot that differs from the current."
  (interactive)
  (cl-loop for pos = (progn (forward-line -1) (point))
           while (< (point-min) pos)
           for id = (tabulated-list-get-id)
           for s = (snapshot-timeline-snapshot-by-id id)
           until (and s (snapshot-interestingp s)))
  (snapshot-timeline-sync-timemachine))

(defun snapshot-timeline-restore ()
  "Restore the focused snapshot.
Replace the current file with the snapshotted file's contents.
Prompt the user for confirmation."
  (interactive)
  (snapshot-timemachine-restore-snapshot
   (snapshot-timeline-snapshot-by-id (tabulated-list-get-id))))

;; Minor-mode for timeline

(defvar snapshot-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'snapshot-timeline-show-snapshot-or-diff)
    (define-key map (kbd "SPC") 'snapshot-timeline-view-snapshot)
    (define-key map (kbd "a")   'snapshot-timeline-mark-as-A)
    (define-key map (kbd "b")   'snapshot-timeline-mark-as-B)
    (define-key map (kbd "d")   'snapshot-timeline-show-diff-A-B)
    (define-key map (kbd "e")   'snapshot-timeline-ediff-A-B)
    (define-key map (kbd "i")   'snapshot-timeline-toggle-interesting-only)
    (define-key map (kbd "m")   'snapshot-timeline-emerge-A-B)
    (define-key map (kbd "n")   'snapshot-timeline-goto-next-snapshot)
    (define-key map (kbd "p")   'snapshot-timeline-goto-prev-snapshot)
    (define-key map (kbd "N")   'snapshot-timeline-goto-next-interesting-snapshot)
    (define-key map (kbd "P")   'snapshot-timeline-goto-prev-interesting-snapshot)
    (define-key map (kbd "r")   'snapshot-timeline-restore)
    (define-key map (kbd "u")   'snapshot-timeline-unmark)
    (define-key map (kbd "U")   'snapshot-timeline-unmark-all)
    (define-key map (kbd "v")   'snapshot-timeline-view-snapshot)
    (define-key map (kbd "<")   'snapshot-timeline-goto-start)
    (define-key map (kbd ">")   'snapshot-timeline-goto-end)
    (define-key map (kbd "=")   'snapshot-timeline-show-diff)
    map)
  "Local keymap for `snapshot-timeline-mode' buffers.")

(defun snapshot-timeline-reload ()
  "Reload the snapshots from disk and update the timeline.
Intended for the `tabulated-list-revert-hook' of
`snapshot-timeline-mode'."
  (setq tabulated-list-entries
        (snapshot-timeline-format-snapshots
         (snapshot-timemachine-find-snapshots
          snapshot-timemachine--file)))
  (snapshot-timeline-sync-timemachine))

(defun snapshot-timeline-compare-by-time (s1 s2)
  "Compare snapshot S1 to S2 formatted as `tabulated-list-entries' by time.
Return t when S1 was older than S2, `time-less-p' is used for
comparison.  The snapshots should be formatted by
`snapshot-timeline-format-snapshots' to use as
`tabulated-list-entries'.  This function is intended as a sorter
for the time field in `tabulated-list-format'."
  (cl-labels ((get-time-from-entry (snapshot-entry)
                                   (nth 2 (aref (cadr snapshot-entry) 1))))
    (time-less-p (get-time-from-entry s1) (get-time-from-entry s2))))

(define-derived-mode snapshot-timeline-mode tabulated-list-mode
  "Snapshot Timeline"
  "Display a timeline of snapshots of a file."
  :group 'snapshot-timemachine
  (add-hook 'tabulated-list-revert-hook #'snapshot-timeline-reload nil t)
  (let ((time-width (length
                     (format-time-string
                      snapshot-timemachine-time-format '(0 0 0 0)))))
    (setq tabulated-list-padding 2
          tabulated-list-format
          ;; TODO make widths configurable
          `[("Snapshot" 8 t)
            ("Time" ,time-width snapshot-timeline-compare-by-time)
            ("Diffstat" 40 nil)])
    (tabulated-list-init-header)))

;; Timeline launcher

(defun snapshot-timeline-create (file snapshots)
  "Create and return a snapshot timeline buffer.
The snapshot timeline will be of FILE using SNAPSHOTS."
  (let ((timeline-buffer
         (format "timeline:%s" (file-name-nondirectory file))))
    (with-current-buffer (get-buffer-create timeline-buffer)
      (snapshot-timeline-mode)
      (setq snapshot-timemachine--file file
            snapshot-timemachine--snapshots snapshots
            tabulated-list-entries
            (snapshot-timeline-format-snapshots
             snapshots))
      (tabulated-list-print)
      (hl-line-mode 1)
      (switch-to-buffer timeline-buffer))))

;;;###autoload
(defun snapshot-timeline (&optional file)
  "Display a timeline of snapshots of FILE.
FILE defaults to the file the current buffer is visiting."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (if (null file)
        (message "Buffer is not visiting a file")
      (let ((snapshots (snapshot-timemachine-find-snapshots file)))
        (if (null snapshots)
            (message "No snapshots found")
          (snapshot-timeline-create file snapshots))))))


(provide 'snapshot-timemachine)
;;; snapshot-timemachine.el ends here
