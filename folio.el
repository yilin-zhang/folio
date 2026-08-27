;;; folio.el --- Enhanced bookmark management -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Yilin Zhang
;; Maintainer: Yilin Zhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: convenience, bookmarks
;; URL: https://github.com/yilin-zhang/folio

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Folio is a plugin complementary to the built-in bookmark, with alternative
;; list UI and web URL support.

;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'url-parse)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'thingatpt)

;; nerd-icons is an optional runtime dependency, probed by
;; `folio--nerd-icons-available-p'.  Declare it so byte compilation stays
;; clean without pulling the package in as a hard requirement.
(declare-function nerd-icons-octicon "nerd-icons" (icon-name &rest args))

(defgroup folio nil
  "Bookmark enhancement for Emacs."
  :group 'convenience)

(defcustom folio-url-open-function #'browse-url
  "Function used to open URLs."
  :type 'function)

(defcustom folio-list-sort-key 'added
  "Sort key for folio list.
Valid values are \\='added or \\='title."
  :type '(choice (const :tag "Added time" added)
                 (const :tag "Title" title)))

(defconst folio--status-unread "unread"
  "Status string for unread entries.")

(defconst folio--status-read "read"
  "Status string for read entries.")

(defconst folio--cold-cache (make-symbol "folio-cold-cache")
  "Sentinel used when the Folio entry cache is cold.")

(defvar folio--list-entries folio--cold-cache
  "Cached entries built from `bookmark-alist'.")

(defvar-local folio--filter-tags nil
  "Tag filter list for the current list buffer, or nil for no filter.")

(defvar-local folio--note-edit-id nil
  "Entry ID for the current note-edit buffer.")

(defvar-local folio--note-edit-entry nil
  "Entry data for the current note-edit buffer.")

(defvar-local folio-list--marked nil
  "Hash table of marked entry IDs in the current list buffer.
Initialized by `folio-list-mode'.")

(defvar-local folio-list--mark-overlays nil
  "Hash table mapping marked entry ID to its (row . indicator) overlay pair.
Initialized by `folio-list-mode'.")

(defvar folio--location-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'folio-list-open)
    (define-key map (kbd "RET") #'folio-list-open)
    map)
  "Keymap for clickable location field.")

(defvar folio--tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'folio-list-filter-tag-at-point)
    map)
  "Keymap for clickable tag field.")

;;;; Faces

(defface folio-title-face
  '((t :weight bold))
  "Face for folio titles."
  :group 'folio)

(defface folio-unread-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the unread marker in the list."
  :group 'folio)

(defface folio-type-url-face
  '((t :inherit font-lock-string-face))
  "Face for URL type entries."
  :group 'folio)

(defface folio-type-file-face
  '((t :inherit font-lock-variable-name-face))
  "Face for file type entries."
  :group 'folio)

(defface folio-tags-face
  '((t :inherit font-lock-type-face))
  "Face for folio tags."
  :group 'folio)

(defface folio-location-face
  '((t :inherit shadow))
  "Face for folio location."
  :group 'folio)

(defface folio-note-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for folio note marker in the list."
  :group 'folio)

(defface folio-timestamp-face
  '((t :inherit font-lock-constant-face))
  "Face for folio added time."
  :group 'folio)

(defface folio-list-mark-face
  '((((background dark)) (:background "DarkGoldenrod4"))
    (t (:background "LightYellow1")))
  "Face for marked rows in the folio list."
  :group 'folio)

(defface folio-list-mark-indicator-face
  '((t :inherit warning))
  "Face for the mark indicator character in the folio list."
  :group 'folio)

;;;; Cache

(defun folio--ensure-bookmarks-loaded ()
  "Ensure the Emacs bookmark database is loaded."
  (bookmark-maybe-load-default-file))

(defun folio--invalidate-cache ()
  "Clear the cached entries list."
  (setq folio--list-entries folio--cold-cache))

(defun folio--entries ()
  "Return cached folio entries, rebuilding from bookmarks when cold."
  (folio--ensure-bookmarks-loaded)
  (when (eq folio--list-entries folio--cold-cache)
    (setq folio--list-entries (folio--bookmarks->db)))
  folio--list-entries)

(defun folio--refresh-db ()
  "Invalidate the cache and reload entries from bookmarks."
  (folio--invalidate-cache)
  (folio--entries))

(defun folio--bookmarks->db ()
  "Return a fresh list of folio entries from `bookmark-alist'."
  (mapcar (lambda (bm)
            (folio--bookmark-record->entry
             (bookmark-name-from-full-record bm)
             (bookmark-get-bookmark-record bm)))
          bookmark-alist))

;;;; Entry model

(defun folio--new-id ()
  "Return a reasonably unique ID string."
  (format "%s-%06x"
          (format-time-string "%Y%m%d%H%M%S")
          (random #xFFFFFF)))

(defun folio--entry-status (entry)
  "Return the status string for ENTRY."
  (or (alist-get 'status entry) ""))

(defun folio--entry-unread-p (entry)
  "Return non-nil when ENTRY is unread."
  (string= (folio--entry-status entry) folio--status-unread))

(defun folio--entry-read-p (entry)
  "Return non-nil when ENTRY is read."
  (string= (folio--entry-status entry) folio--status-read))

(defun folio--find-entry (id)
  "Return the entry with ID, or nil."
  (seq-find (lambda (entry)
              (string= id (alist-get 'id entry)))
            (folio--entries)))

(defun folio--bookmark-record->entry (name record)
  "Convert bookmark NAME and RECORD into a folio entry."
  (let* ((type (cond
                ((alist-get 'url record) "url")
                ((alist-get 'filename record) "file")
                (t "bookmark")))
         (folio-id (alist-get 'folio-id record))
         (added (or (alist-get 'folio-added record)
                    (when (and (alist-get 'last-modified record)
                               (fboundp 'bookmark-time-to-time))
                      (format-time-string "%Y-%m-%d %H:%M"
                                          (bookmark-time-to-time
                                           (alist-get 'last-modified record))))))
         (entry `((id . ,(or folio-id name))
                  (folio-id . ,folio-id)
                  (bookmark . ,name)
                  (type . ,type)
                  (title . ,name)
                  (handler . ,(alist-get 'handler record))
                  (record . ,(copy-sequence record))
                  (tags . ,(alist-get 'folio-tags record))
                  (note . ,(alist-get 'annotation record))
                  (status . ,(or (alist-get 'folio-status record)
                                 folio--status-unread))
                  (added . ,added))))
    (pcase type
      ("url"  (setf (alist-get 'url entry)  (alist-get 'url record)))
      ("file" (setf (alist-get 'path entry) (alist-get 'filename record))))
    entry))

(defun folio--entry->bookmark-record (entry)
  "Convert ENTRY into a bookmark record, or nil for an unknown type."
  (let* ((type (alist-get 'type entry))
         (handler (alist-get 'handler entry))
         (record (pcase type
                   ("url"
                    `((url . ,(alist-get 'url entry))
                      (handler . ,(or handler #'folio-bookmark-url-handler))))
                   ("file"
                    `((filename . ,(alist-get 'path entry))
                      (handler . ,(or handler #'bookmark-default-handler))))
                   ("bookmark"
                    (or (copy-sequence (alist-get 'record entry))
                        (when handler `((handler . ,handler)))))
                   (_ nil))))
    (when record
      (let ((id (alist-get 'id entry))
            (tags (alist-get 'tags entry))
            (note (alist-get 'note entry))
            (status (alist-get 'status entry))
            (added (alist-get 'added entry)))
        (when id (push (cons 'folio-id (copy-sequence id)) record))
        (push (cons 'folio-tags (copy-sequence tags)) record)
        (unless (string-blank-p (or note ""))
          (push (cons 'annotation (copy-sequence note)) record))
        (when status (push (cons 'folio-status status) record))
        (when added (push (cons 'folio-added added) record))))
    record))

;;;; Merge helpers

(defun folio--merge-record-allow-remove (base-record updated-record &rest allow-remove-keys)
  "Merge UPDATED-RECORD into BASE-RECORD.
Keys listed in ALLOW-REMOVE-KEYS are deleted when their updated value
is nil instead of being set to nil."
  (let ((record (copy-sequence base-record)))
    (dolist (pair updated-record)
      (let ((key (car pair))
            (value (cdr pair)))
        (if (and (memq key allow-remove-keys) (null value))
            (setq record (assq-delete-all key record))
          (setf (alist-get key record) value))))
    record))

(defun folio--merge-record-if-missing (base-record updated-record)
  "Merge UPDATED-RECORD into BASE-RECORD only for keys absent from BASE-RECORD."
  (let ((record (copy-sequence base-record)))
    (dolist (pair updated-record)
      (unless (assoc (car pair) record)
        (push (cons (car pair) (cdr pair)) record)))
    record))

;;;; Bookmark lookup helpers

(defun folio--bookmark->name+record (bookmark)
  "Return (NAME . RECORD) for BOOKMARK, or nil."
  (let* ((name (cond
                ((stringp bookmark) bookmark)
                ((consp bookmark)
                 (bookmark-name-from-full-record bookmark))))
         (record (and name (bookmark-get-bookmark-record bookmark))))
    (when (and name record)
      (cons name record))))

(defun folio--bookmark-name-for-id (id)
  "Return the bookmark name for folio ID, or nil."
  (or (car (seq-find (lambda (bm)
                       (equal id (alist-get 'folio-id (cdr bm))))
                     bookmark-alist))
      (car (seq-find (lambda (bm)
                       (and (null (alist-get 'folio-id (cdr bm)))
                            (equal id (car bm))))
                     bookmark-alist))))

(defun folio--unique-bookmark-name (base &optional existing-name)
  "Return a unique bookmark name based on BASE.
EXISTING-NAME is allowed to match BASE without forcing a suffix."
  (let ((name base)
        (n 2))
    (while (and (bookmark-get-bookmark name t)
                (not (string= name existing-name)))
      (setq name (format "%s (%d)" base n))
      (setq n (1+ n)))
    name))

;;;; Write paths

(defun folio--store-entry-as-bookmark (entry &optional name)
  "Store new ENTRY as a bookmark.  Return the bookmark name."
  (folio--ensure-bookmarks-loaded)
  (let* ((title (string-trim (or (alist-get 'title entry) "")))
         (base-name (if (string-blank-p title)
                        (or (alist-get 'url entry)
                            (alist-get 'path entry)
                            "Untitled")
                      title))
         (bm-name (or name (folio--unique-bookmark-name base-name)))
         (record (folio--entry->bookmark-record entry)))
    (when record
      (bookmark-store bm-name record nil))
    bm-name))

(defun folio--store-entry-with-name (entry name &optional old-name)
  "Store ENTRY as bookmark NAME, removing OLD-NAME when it differs."
  (folio--ensure-bookmarks-loaded)
  (let* ((existing (bookmark-get-bookmark name t))
         (note (alist-get 'note entry))
         (note-blank (string-blank-p (or note "")))
         (record (folio--entry->bookmark-record entry)))
    (when (and existing note-blank)
      (push (cons 'annotation nil) record))
    (let ((merged (when (and record existing)
                    (folio--merge-record-allow-remove
                     (bookmark-get-bookmark-record existing)
                     record
                     'annotation
                     'folio-tags))))
      (bookmark-store name (or merged record) nil)
      (when (and old-name (not (string= old-name name)))
        (bookmark-delete old-name t)))))

(defun folio--save-entry (id entry)
  "Persist ENTRY for ID without refreshing the list."
  (folio--ensure-bookmarks-loaded)
  (let* ((name (or (alist-get 'bookmark entry)
                   (folio--bookmark-name-for-id id)))
         (record (folio--entry->bookmark-record entry)))
    (when (and name record)
      (folio--store-entry-with-name entry name))))

(defun folio--delete-entry (id)
  "Delete entry with ID."
  (folio--ensure-bookmarks-loaded)
  (let ((name (folio--bookmark-name-for-id id)))
    (when name
      (bookmark-delete name t))))

(defun folio--commit-entry (id entry)
  "Persist ENTRY for ID and refresh the list."
  (folio--save-entry id entry)
  (folio-list-refresh))

;;;; Tags

(defun folio--clean-tags (tags)
  "Normalize TAGS: trim, downcase, drop blanks, deduplicate, sort."
  (let* ((cleaned (mapcar (lambda (tag) (downcase (string-trim tag))) tags))
         (non-blank (seq-filter (lambda (tag) (not (string-blank-p tag))) cleaned))
         (unique (seq-uniq non-blank #'string=)))
    (sort unique #'string-lessp)))

(defun folio--read-tags (&optional initial-tags)
  "Prompt for a list of tags.  INITIAL-TAGS seed the input."
  (let* ((choices (folio--all-tags))
         (initial (when initial-tags (folio--format-tags initial-tags)))
         (tags (completing-read-multiple
                "Tags: "
                choices nil nil initial nil nil)))
    (folio--clean-tags tags)))

(defun folio--format-tags (tags)
  "Format TAGS list for display."
  (if tags
      (string-join tags ",")
    ""))

(defun folio--format-tags-clickable (tags)
  "Format TAGS list with clickable text."
  (when tags
    (mapconcat
     (lambda (tag)
       (propertize tag
                   'face 'folio-tags-face
                   'mouse-face 'highlight
                   'help-echo "Filter by this tag"
                   'keymap folio--tag-map
                   'folio-tag tag))
     tags
     ",")))

(defun folio--all-tags ()
  "Return a sorted unique list of all tags in the database."
  ;; copy-sequence: mapcan splices its lists, so without copying it would
  ;; mutate the per-entry tags lists.
  (sort (seq-uniq (mapcan (lambda (entry)
                            (copy-sequence (alist-get 'tags entry)))
                          (folio--entries)))
        #'string-lessp))

;;;; Filtering and sorting

(defun folio--matches-filter (entry)
  "Return non-nil if ENTRY matches the current filter."
  (let ((tags (alist-get 'tags entry)))
    (if folio--filter-tags
        (seq-every-p (lambda (tag) (member tag tags)) folio--filter-tags)
      t)))

(defun folio--entry< (a b)
  "Compare entries A and B for list sorting."
  (if (eq folio-list-sort-key 'title)
      (let ((ta (downcase (or (alist-get 'title a) "")))
            (tb (downcase (or (alist-get 'title b) ""))))
        (if (string= ta tb)
            (string< (or (alist-get 'added b) "")
                     (or (alist-get 'added a) ""))
          (string< ta tb)))
    (string< (or (alist-get 'added b) "")
             (or (alist-get 'added a) ""))))

;;;; URL helpers

(defun folio--current-url ()
  "Return a URL near point, or from the kill ring, or nil."
  (or (thing-at-point 'url t)
      (and (stringp (car kill-ring))
           (string-match-p "^https?://" (car kill-ring))
           (car kill-ring))))

(defun folio--normalize-url (url)
  "Trim URL and add an https scheme when none is present."
  (let ((clean-url (when (stringp url) (string-trim url))))
    (when (and (stringp clean-url) (not (string-blank-p clean-url)))
      (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*://" clean-url)
          clean-url
        (concat "https://" clean-url)))))

(defun folio--guess-title-from-url (url)
  "Guess a reasonable title from URL using host/path heuristics."
  (let* ((parsed (url-generic-parse-url url))
         (host (or (url-host parsed) ""))
         (path (or (url-filename parsed) ""))
         (path (or (car (split-string path "[?#]" t)) ""))
         (path (string-trim-right path "/"))
         (parts (seq-filter (lambda (part) (not (string= part "")))
                            (split-string host "\\.")))
         (tlds '("com" "edu" "org" "net" "io" "dev" "app" "ai" "co"
                 "gov" "mil" "info" "me" "us" "uk")))
    (when (and parts (string= (car parts) "www"))
      (setq parts (cdr parts)))
    (when (and parts (member (car (last parts)) tlds))
      (setq parts (butlast parts)))
    (let* ((host-base (string-join parts "."))
           (segments (seq-filter (lambda (seg) (not (string= seg "")))
                                 (split-string path "/")))
           (last-seg (car (last segments)))
           (last-seg (or last-seg ""))
           (last-seg (replace-regexp-in-string
                      "\\.[a-zA-Z0-9]+\\'" "" last-seg))
           (last-seg (replace-regexp-in-string "[-_]+" " " last-seg)))
      (if (and (not (string-blank-p last-seg))
               (not (string-match-p "\\`index\\'" (downcase last-seg))))
          last-seg
        (if (string-blank-p host-base) url host-base)))))

;;;; Capture

(defun folio--capture-entry (type title-default &rest fields)
  "Capture entry data with TYPE, TITLE-DEFAULT, and extra FIELDS."
  (let* ((title (read-string "Title: " title-default))
         (tags (folio--read-tags))
         (note (read-string "Note (optional): "))
         (entry `((id . ,(folio--new-id))
                  (type . ,type)
                  (title . ,title)
                  (tags . ,tags)
                  (note . ,(unless (string-blank-p note) note))
                  (status . ,folio--status-unread)
                  (added . ,(format-time-string "%Y-%m-%d %H:%M")))))
    (dolist (pair fields)
      (setf (alist-get (car pair) entry) (cdr pair)))
    entry))

;;;; Nerd icons

(defun folio--nerd-icons-available-p ()
  "Return non-nil when nerd-icons is loadable."
  (require 'nerd-icons nil t))

(defun folio--entry-icon (entry)
  "Return a Nerd Icon for ENTRY, or nil if nerd-icons is unavailable."
  (when (folio--nerd-icons-available-p)
    (pcase (alist-get 'type entry)
      ("file"
       (let ((path (alist-get 'path entry)))
         (if (and path (fboundp 'nerd-icons-icon-for-file))
             (nerd-icons-icon-for-file
              (file-name-nondirectory path)
              :face 'folio-type-file-face)
           (nerd-icons-octicon "nf-oct-file" :face 'folio-type-file-face))))
      ("bookmark"
       (nerd-icons-octicon "nf-oct-bookmark" :face 'folio-type-url-face))
      (_
       (nerd-icons-octicon "nf-oct-link" :face 'folio-type-url-face)))))

(defun folio--type-letter (entry)
  "Return a single-letter type indicator for ENTRY."
  (let* ((type-text (or (alist-get 'type entry) ""))
         (face (if (string= type-text "file")
                   'folio-type-file-face
                 'folio-type-url-face))
         (letter (pcase type-text
                   ("file"     "F")
                   ("bookmark" "B")
                   (_          "U"))))
    (propertize letter 'face face)))

;;;; List display

(defun folio--list-format (title-w tags-w)
  "Return the tabulated-list format with the given dynamic column widths."
  (vector (list "Added" 16 t)
          (list "Title" title-w t)
          (list "Tags" tags-w t)
          (list "Unread" 6 t)
          (list "Note" 4 t)
          (list "Location" 36 t)))

(defun folio--entry->row (entry)
  "Convert ENTRY to a tabulated-list row."
  (let* ((location-text (or (alist-get 'url entry)
                            (alist-get 'path entry)
                            ""))
         (unread (folio--entry-unread-p entry))
         (title-text (or (alist-get 'title entry) ""))
         (type-indicator (or (folio--entry-icon entry)
                             (folio--type-letter entry)))
         (title (concat type-indicator " "
                        (propertize title-text 'face 'folio-title-face)))
         (location (propertize (truncate-string-to-width location-text 36 nil nil "...")
                               'face 'folio-location-face
                               'mouse-face 'highlight
                               'help-echo "Open this entry"
                               'keymap folio--location-map))
         (tags (or (folio--format-tags-clickable (alist-get 'tags entry)) ""))
         (unread-flag (if unread
                          (propertize "*" 'face 'folio-unread-face)
                        ""))
         (note-text (alist-get 'note entry))
         (note (if (string-blank-p (or note-text ""))
                   ""
                 (propertize "+" 'face 'folio-note-face)))
         (added (propertize (or (alist-get 'added entry) "")
                            'face 'folio-timestamp-face)))
    (add-text-properties 0 (length title)
                         (list 'mouse-face 'highlight
                               'help-echo "Open this entry"
                               'keymap folio--location-map)
                         title)
    (list (alist-get 'id entry)
          (vector added title tags unread-flag note location))))

(defun folio-list-refresh ()
  "Refresh the folio list buffer."
  (interactive)
  (let ((entries (seq-sort
                  #'folio--entry<
                  (seq-filter #'folio--matches-filter (folio--refresh-db))))
        (title-w 5) (tags-w 4)
        rows)
    (dolist (entry entries)
      (let ((row (folio--entry->row entry)))
        ;; Measure widths from the built row to avoid reformatting.
        ;; Row vector: [added title tags unread note location]
        (let ((vec (cadr row)))
          (setq title-w (max title-w (string-width (aref vec 1)))
                tags-w  (max tags-w  (string-width (aref vec 2)))))
        (push row rows)))
    (setq tabulated-list-format (folio--list-format title-w tags-w))
    (tabulated-list-init-header)
    (setq tabulated-list-sort-key nil)
    (setq tabulated-list-entries (nreverse rows)))
  (tabulated-list-print t)
  (folio-list--apply-marks))

(defun folio-list--goto-id (id)
  "Move point to the row whose tabulated-list ID equals ID."
  (when id
    (let (position)
      (save-excursion
        (goto-char (point-min))
        (while (and (not position) (not (eobp)))
          (when (equal (tabulated-list-get-id) id)
            (setq position (point)))
          (forward-line 1)))
      (when position
        (goto-char position)))))

(defun folio-list--nearest-surviving-id (deleted-ids)
  "Return the nearest row ID not listed in DELETED-IDS.
Prefer a following row when two surviving rows are equally near point."
  (let ((origin-line (line-number-at-pos))
        best-id best-distance best-forward)
    (save-excursion
      (goto-char (point-min))
      (let ((line 1))
        (while (not (eobp))
          (let ((id (tabulated-list-get-id)))
            (when (and id (not (member id deleted-ids)))
              (let* ((distance (abs (- line origin-line)))
                     (forward (>= line origin-line)))
                (when (or (null best-distance)
                          (< distance best-distance)
                          (and (= distance best-distance)
                               forward
                               (not best-forward)))
                  (setq best-id id
                        best-distance distance
                        best-forward forward)))))
          (forward-line 1)
          (cl-incf line))))
    best-id))

(defun folio--refresh-keep-position (&optional fallback-id)
  "Refresh the list and restore point to its entry or FALLBACK-ID."
  (let* ((id (tabulated-list-get-id))
         (window (get-buffer-window (current-buffer) t))
         (start (and window (window-start window))))
    (folio-list-refresh)
    (or (folio-list--goto-id id)
        (folio-list--goto-id fallback-id))
    (when (and window (window-live-p window))
      (set-window-start window start))))

(defun folio--refresh-list-buffer (&optional keep-position)
  "Refresh the folio list buffer if it exists.
When KEEP-POSITION is non-nil and the buffer is visible, preserve its view."
  (when-let* ((buffer (get-buffer "*Folio*")))
    (with-current-buffer buffer
      (when (derived-mode-p 'folio-list-mode)
        (if keep-position
            (folio--refresh-keep-position)
          (folio-list-refresh))))))

;;;; Entry at point

(defun folio--entry-at-point ()
  "Return (ID . ENTRY) at point."
  (let* ((id (tabulated-list-get-id))
         (entry (and id (folio--find-entry id))))
    (unless entry
      (user-error "Folio: no entry at point"))
    (cons id entry)))

(cl-defmacro folio--with-entry-at-point ((id entry) &rest body)
  "Bind ID and ENTRY to the item at point, then run BODY."
  (declare (indent 1))
  `(let* ((pair (folio--entry-at-point))
          (,id (car pair))
          (,entry (cdr pair)))
     ,@body))

;;;; List commands

(defun folio-list-open ()
  "Open the entry at point."
  (interactive)
  (folio--with-entry-at-point (_id entry)
    (let ((name (alist-get 'bookmark entry)))
      (if name
          (bookmark-jump name)
        (message "Folio: no bookmark name for entry")))))

(defun folio-list-toggle-read ()
  "Toggle read/unread status for entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (let ((new-status (if (folio--entry-read-p entry)
                          folio--status-unread
                        folio--status-read)))
      (setf (alist-get 'status entry) new-status)
      (folio--commit-entry id entry))))

(defun folio-list-edit-tags ()
  "Edit tags for marked entries, or for the entry at point.
With marks: prompt for a tag list and replace the tags on each marked
entry after confirmation.  Without marks: edit the entry at point, seeded
with its current tags."
  (interactive)
  (let ((marked-ids (folio-list--marked-ids)))
    (if marked-ids
        (let ((tags (folio--read-tags))
              (entries (folio--entries)))
          (when (yes-or-no-p
                 (format "Set tags %s on %d marked entries? "
                         (if tags (folio--format-tags tags) "(none)")
                         (length marked-ids)))
            (dolist (id marked-ids)
              (when-let* ((entry (seq-find
                                  (lambda (candidate)
                                    (equal id (alist-get 'id candidate)))
                                  entries)))
                (setf (alist-get 'tags entry) tags)
                (folio--save-entry id entry)))
            (folio-list--clear-marks)
            (folio-list-refresh)
            (message "Folio: tagged %d %s"
                     (length marked-ids)
                     (if (= (length marked-ids) 1) "entry" "entries"))))
      (folio--with-entry-at-point (id entry)
        (let ((tags (folio--read-tags (alist-get 'tags entry))))
          (setf (alist-get 'tags entry) tags)
          (folio--commit-entry id entry))))))

(defun folio-list-edit-title ()
  "Edit title for entry at point."
  (interactive)
  (folio--with-entry-at-point (_id entry)
    (let* ((current (or (alist-get 'title entry) ""))
           (title (string-trim (read-string "Title: " current)))
           (title (if (string-blank-p title) current title))
           (old-name (alist-get 'bookmark entry))
           (new-name (folio--unique-bookmark-name title old-name)))
      (setf (alist-get 'title entry) title)
      (setf (alist-get 'bookmark entry) new-name)
      (folio--store-entry-with-name entry new-name old-name)
      (folio--refresh-list-buffer))))

(defun folio-list-edit-note ()
  "Edit note for entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (let ((buf (get-buffer-create "*Folio Note*"))
          (current (or (alist-get 'note entry) "")))
      (with-current-buffer buf
        (erase-buffer)
        (insert current)
        (goto-char (point-min))
        (folio-note-edit-mode)
        (setq folio--note-edit-id id)
        (setq folio--note-edit-entry entry)
        (setq header-line-format "Edit note. C-c C-c to apply, C-c C-k to cancel."))
      (pop-to-buffer buf))))

(defun folio-list-edit-location ()
  "Edit URL or file path for entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (pcase (alist-get 'type entry)
      ("url"
       (let* ((current (or (alist-get 'url entry) ""))
              (url (folio--normalize-url (read-string "URL: " current))))
         (unless url
           (user-error "Folio: URL cannot be empty"))
         (setf (alist-get 'url entry) url)
         (folio--commit-entry id entry)))
      ("file"
       (let* ((current (or (alist-get 'path entry) ""))
              (path (read-file-name "File: " nil current t)))
         (setf (alist-get 'path entry) (expand-file-name path))
         (folio--commit-entry id entry)))
      (_ (message "Folio: unknown entry type")))))

(defun folio-list-delete ()
  "Delete entry at point, or all marked entries when any are marked."
  (interactive)
  (let ((ids (folio-list--marked-ids)))
    (if ids
        (when (yes-or-no-p (format "Delete %d marked entries? " (length ids)))
          (let ((fallback-id (folio-list--nearest-surviving-id ids)))
            (dolist (id ids)
              (folio--delete-entry id))
            (folio-list--clear-marks)
            (folio--refresh-keep-position fallback-id)
            (message "Folio: deleted %d entries" (length ids))))
      (folio--with-entry-at-point (id entry)
        (let ((name (or (alist-get 'bookmark entry)
                        (folio--bookmark-name-for-id id))))
          (cond
           ((not name)
            (message "Folio: no bookmark name for entry"))
           ((y-or-n-p "Delete this entry? ")
            (let ((fallback-id (folio-list--nearest-surviving-id (list id))))
              (folio--delete-entry id)
              (folio--refresh-keep-position fallback-id)))
           (t (message "Folio: delete canceled"))))))))

(defun folio-list-filter-tags (tags)
  "Filter the list by TAGS (intersection)."
  (interactive
   (list
    (let ((choices (folio--all-tags)))
      (completing-read-multiple
       "Tags: "
       choices nil t nil nil nil))))
  (setq folio--filter-tags tags)
  (folio-list-refresh))

(defun folio-list-filter-tag-at-point (event)
  "Filter the list by the tag at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (let ((tag (get-text-property (point) 'folio-tag)))
    (if tag
        (progn
          (setq folio--filter-tags (list tag))
          (folio-list-refresh))
      (message "Folio: no tag at point"))))

(defun folio-list-sort-by-title ()
  "Sort list by title."
  (interactive)
  (setq folio-list-sort-key 'title)
  (folio-list-refresh)
  (message "Folio: sorted by title"))

(defun folio-list-sort-by-time ()
  "Sort list by added time."
  (interactive)
  (setq folio-list-sort-key 'added)
  (folio-list-refresh)
  (message "Folio: sorted by time"))

;;;; Mark and bulk operations

(defun folio-list--marked-ids ()
  "Return the list of marked entry IDs in the current buffer."
  (when folio-list--marked
    (hash-table-keys folio-list--marked)))

(defun folio-list--delete-mark-overlays (ovs)
  "Delete overlay pair OVS (a cons of two overlays)."
  (when (overlayp (car ovs)) (delete-overlay (car ovs)))
  (when (overlayp (cdr ovs)) (delete-overlay (cdr ovs))))

(defun folio-list--clear-marks ()
  "Remove all marks and their overlays in the current buffer."
  (when folio-list--mark-overlays
    (maphash (lambda (_id ovs) (folio-list--delete-mark-overlays ovs))
             folio-list--mark-overlays)
    (clrhash folio-list--mark-overlays))
  (when folio-list--marked
    (clrhash folio-list--marked)))

(defun folio-list--add-mark-overlay (id)
  "Highlight the current line as marked for ID."
  (when-let* ((existing (gethash id folio-list--mark-overlays)))
    (folio-list--delete-mark-overlays existing))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position)))
        (mark-ov (make-overlay (line-beginning-position)
                               (1+ (line-beginning-position)))))
    (overlay-put ov 'face 'folio-list-mark-face)
    (overlay-put mark-ov 'display
                 (propertize "*" 'face 'folio-list-mark-indicator-face))
    (puthash id (cons ov mark-ov) folio-list--mark-overlays)))

(defun folio-list--apply-marks ()
  "Reapply mark overlays after a buffer refresh.
Drops existing overlays and rebuilds them at each marked entry's new
line position, so marks survive `folio-list-refresh'."
  (when folio-list--mark-overlays
    (maphash (lambda (_id ovs) (folio-list--delete-mark-overlays ovs))
             folio-list--mark-overlays)
    (clrhash folio-list--mark-overlays))
  (when folio-list--marked
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((id (tabulated-list-get-id)))
          (when (and id (gethash id folio-list--marked))
            (folio-list--add-mark-overlay id)))
        (forward-line 1)))))

(defun folio-list-mark ()
  "Toggle mark on the entry at point and advance to the next line.
With an active region, mark all entries in the region (no toggle)."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             ;; When the region ends at the beginning of a line, the user
             ;; visually selected up to but not including that line.  Back
             ;; up by one char so the loop does not mark that extra row.
             (finish (if (and (> end beg)
                              (save-excursion (goto-char end) (bolp)))
                         (1- end)
                       end)))
        (save-excursion
          (goto-char beg)
          (beginning-of-line)
          (while (<= (line-beginning-position) finish)
            (when-let* ((id (tabulated-list-get-id)))
              (puthash id t folio-list--marked)
              (folio-list--add-mark-overlay id))
            (forward-line 1)))
        (deactivate-mark)
        (goto-char (max beg end))
        (beginning-of-line)
        (forward-line 1))
    (let ((id (tabulated-list-get-id)))
      (unless id (user-error "Folio: no entry on this line"))
      (if (gethash id folio-list--marked)
          (progn
            (remhash id folio-list--marked)
            (when-let* ((ovs (gethash id folio-list--mark-overlays)))
              (folio-list--delete-mark-overlays ovs)
              (remhash id folio-list--mark-overlays)))
        (puthash id t folio-list--marked)
        (folio-list--add-mark-overlay id))
      (forward-line 1))))

(defun folio-list-unmark ()
  "Unmark the entry at point and advance to the next line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "Folio: no entry on this line"))
    (remhash id folio-list--marked)
    (when-let* ((ovs (gethash id folio-list--mark-overlays)))
      (folio-list--delete-mark-overlays ovs)
      (remhash id folio-list--mark-overlays))
    (forward-line 1)))

(defun folio-list-unmark-all ()
  "Clear all marks in the current folio list buffer."
  (interactive)
  (folio-list--clear-marks)
  (message "Folio: cleared all marks"))

;;;; Note edit mode

(defun folio--note-edit-apply ()
  "Apply the note in the current buffer to its entry."
  (interactive)
  (unless (and folio--note-edit-id folio--note-edit-entry)
    (user-error "Folio: no entry attached to this buffer"))
  (let* ((text (string-trim-right (buffer-string)))
         (note (unless (string-blank-p text) text))
         (id folio--note-edit-id)
         (entry folio--note-edit-entry))
    (setf (alist-get 'note entry) note)
    (folio--save-entry id entry)
    (folio--refresh-list-buffer t)
    (quit-window t)))

(defun folio--note-edit-cancel ()
  "Cancel note editing without saving."
  (interactive)
  (quit-window t))

(defvar folio-note-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'folio--note-edit-apply)
    (define-key map (kbd "C-c C-k") #'folio--note-edit-cancel)
    map)
  "Keymap for `folio-note-edit-mode'.")

(define-derived-mode folio-note-edit-mode text-mode "Folio-Note"
  "Major mode for editing Folio notes."
  (setq-local require-final-newline nil))

;;;; Main entry points

;;;###autoload
(defun folio-bookmark-url-handler (bookmark)
  "Open a URL from BOOKMARK."
  (let* ((pair (folio--bookmark->name+record bookmark))
         (record (cdr-safe pair))
         (url (and record (alist-get 'url record))))
    (if url
        (funcall folio-url-open-function url)
      (message "Folio: no URL in bookmark"))))

;;;###autoload
(defun folio-add-url (url)
  "Add a URL bookmark.
URL is read from the minibuffer with a helpful default."
  (interactive
   (list (read-string "URL: " (folio--current-url))))
  (folio--ensure-bookmarks-loaded)
  (let* ((url (or (folio--normalize-url url)
                  (user-error "Folio: URL cannot be empty")))
         (entry (folio--capture-entry
                 "url"
                 (folio--guess-title-from-url url)
                 (cons 'url url))))
    (folio--store-entry-as-bookmark entry)
    (folio--refresh-list-buffer)
    (message "Folio: saved URL")))

;;;###autoload
(defun folio-add-file (file)
  "Add a local file bookmark.
FILE is read from the minibuffer with a helpful default."
  (interactive
   (list (read-file-name "File or directory: "
                         nil (buffer-file-name) nil)))
  (folio--ensure-bookmarks-loaded)
  (let* ((path (expand-file-name file))
         (title-default (file-name-nondirectory (directory-file-name path)))
         (entry (folio--capture-entry
                 "file"
                 title-default
                 (cons 'path path))))
    (folio--store-entry-as-bookmark entry)
    (folio--refresh-list-buffer)
    (message "Folio: saved file")))

;;;###autoload
(defun folio-bookmark-set (name)
  "Create a bookmark at point and eagerly adopt it into Folio.
NAME is the bookmark name, like `bookmark-set'."
  (interactive (list (bookmark-completing-read "Set Folio bookmark: ")))
  (folio--ensure-bookmarks-loaded)
  (bookmark-set name)
  (let ((record (bookmark-get-bookmark name t)))
    (when record
      (let* ((record (bookmark-get-bookmark-record record))
             (now (format-time-string "%Y-%m-%d %H:%M"))
             (updates `((folio-id . ,(folio--new-id))
                        (folio-status . ,folio--status-unread)
                        (folio-added . ,now)))
             (merged (folio--merge-record-if-missing record updates)))
        (bookmark-store name merged nil)
        (folio--refresh-list-buffer)
        (message "Folio: saved bookmark")))))

;;;; List mode

(defvar folio-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'folio-list-open)
    (define-key map (kbd "o") #'folio-list-open)
    (define-key map (kbd "/") #'folio-list-filter-tags)
    (define-key map (kbd "*") #'folio-list-toggle-read)
    (define-key map (kbd "r") #'folio-list-edit-title)
    (define-key map (kbd "+") #'folio-list-edit-note)
    (define-key map (kbd "l") #'folio-list-edit-location)
    (define-key map (kbd "t") #'folio-list-edit-tags)
    (define-key map (kbd "d") #'folio-list-delete)
    (define-key map (kbd "g") #'folio-list-refresh)
    (define-key map (kbd "; l") #'folio-list-sort-by-title)
    (define-key map (kbd "; t") #'folio-list-sort-by-time)
    (define-key map (kbd "-") #'folio-add-url)
    (define-key map (kbd "=") #'folio-add-file)
    (define-key map (kbd "m") #'folio-list-mark)
    (define-key map (kbd "u") #'folio-list-unmark)
    (define-key map (kbd "U") #'folio-list-unmark-all)
    map)
  "Keymap for `folio-list-mode'.")

(define-derived-mode folio-list-mode tabulated-list-mode "Folio"
  "Major mode for listing folio entries."
  (make-local-variable 'folio-list-sort-key)
  ;; Drop any overlays held by a prior init of this buffer before we
  ;; replace the hash tables, otherwise the old overlays leak in the buffer.
  (folio-list--clear-marks)
  (setq folio-list--marked (make-hash-table :test #'equal))
  (setq folio-list--mark-overlays (make-hash-table :test #'equal))
  ;; Placeholder format with min widths; `folio-list-refresh' replaces it
  ;; with widths derived from the actual entries.
  (setq tabulated-list-format (folio--list-format 5 4))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'folio-list-refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun folio-list ()
  "Show the folio list buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Folio*")))
    (with-current-buffer buf
      (folio-list-mode)
      (folio-list-refresh))
    (pop-to-buffer buf)))

;;;; Bookmark integration

(defun folio--bookmark-mark-read (bookmark)
  "Mark BOOKMARK as read, adopting it into folio if needed."
  (let* ((pair (folio--bookmark->name+record bookmark))
         (name (car-safe pair))
         (record (cdr-safe pair)))
    (when record
      (let* ((record (copy-alist record))
             (status (or (alist-get 'folio-status record) ""))
             (needs-adopt (null (alist-get 'folio-id record)))
             (needs-mark (not (string= status folio--status-read))))
        (when (or needs-adopt needs-mark)
          (when needs-adopt
            (let ((now (format-time-string "%Y-%m-%d %H:%M")))
              (setf (alist-get 'folio-id record) (folio--new-id))
              (setf (alist-get 'folio-added record) now)))
          (setf (alist-get 'folio-status record) folio--status-read)
          (bookmark-store name record nil)
          ;; Skip the redraw when no window is showing the list buffer; the
          ;; cache invalidation advice has already marked it stale, and the
          ;; next `folio-list' call will rebuild from fresh data.
          (let ((buf (get-buffer "*Folio*")))
            (when (and buf (get-buffer-window buf t))
              (folio--refresh-list-buffer))))))))

(defun folio--bookmark-external-p (bookmark)
  "Return non-nil when BOOKMARK doesn't jump to a buffer."
  (let* ((pair (folio--bookmark->name+record bookmark))
         (record (cdr pair))
         (handler (alist-get 'handler record)))
    (and (not (alist-get 'filename record))
         (not (alist-get 'buffer-name record))
         (or (alist-get 'url record)
             (and handler
                  (not (eq handler #'bookmark-default-handler)))))))

(defun folio--bookmark-clear-fringe-mark ()
  "Remove any bookmark fringe mark at point."
  (dolist (overlay (overlays-in (pos-bol) (1+ (pos-bol))))
    (when (eq (overlay-get overlay 'category) 'bookmark)
      (delete-overlay overlay))))

(defun folio--bookmark-after-jump ()
  "Remove fringe mark for external bookmarks."
  (when (and (stringp bookmark-current-bookmark)
             (folio--bookmark-external-p bookmark-current-bookmark))
    (folio--bookmark-clear-fringe-mark)))

(defun folio--bookmark-jump-advice (orig bookmark &rest args)
  "Mark Folio BOOKMARK as read after `bookmark-jump'.
ORIG is the original function and ARGS are its arguments."
  (let ((external-p (folio--bookmark-external-p bookmark)))
    (let ((bookmark-fringe-mark (if external-p nil bookmark-fringe-mark)))
      (let ((result (apply orig bookmark args)))
        (folio--bookmark-mark-read bookmark)
        result))))

(defun folio--bookmark-change-advice (&rest _args)
  "Invalidate Folio cache when bookmarks change."
  (folio--invalidate-cache))

(unless (advice-member-p #'folio--bookmark-jump-advice 'bookmark-jump)
  (advice-add 'bookmark-jump :around #'folio--bookmark-jump-advice))

(unless (advice-member-p #'folio--bookmark-change-advice 'bookmark-store)
  (advice-add 'bookmark-store :after #'folio--bookmark-change-advice))

(unless (advice-member-p #'folio--bookmark-change-advice 'bookmark-delete)
  (advice-add 'bookmark-delete :after #'folio--bookmark-change-advice))

(unless (advice-member-p #'folio--bookmark-change-advice 'bookmark-load)
  (advice-add 'bookmark-load :after #'folio--bookmark-change-advice))

(add-hook 'bookmark-after-jump-hook #'folio--bookmark-after-jump)

(defun folio-unload-function ()
  "Remove Folio advice and hooks when `unload-feature' is called."
  (advice-remove 'bookmark-jump   #'folio--bookmark-jump-advice)
  (advice-remove 'bookmark-store  #'folio--bookmark-change-advice)
  (advice-remove 'bookmark-delete #'folio--bookmark-change-advice)
  (advice-remove 'bookmark-load #'folio--bookmark-change-advice)
  (remove-hook 'bookmark-after-jump-hook #'folio--bookmark-after-jump)
  nil)

(provide 'folio)
;;; folio.el ends here
