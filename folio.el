;;; folio.el --- Bookmark enhancement for Emacs -*- lexical-binding: t; -*-

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
;;
;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'url)
(require 'url-parse)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defgroup folio nil
  "Pocket-like bookmarks for Emacs."
  :group 'convenience)

(defcustom folio-url-open-function
  #'browse-url
  "Function used to open URLs."
  :type 'function)

(defvar folio--loaded-p nil
  "Non-nil when the database has been loaded.")

(defvar folio--filter-tags nil
  "Current tag filter list, or nil.")

(defcustom folio-list-sort-key 'added
  "Sort key for folio list.
Valid values are \\='added or \\='title."
  :type '(choice (const :tag "Added time" added)
                 (const :tag "Title" title)))

(defcustom folio-nerd-icons-enabled nil
  "When non-nil, display Nerd Font icons in the folio list."
  :type 'boolean)

(defcustom folio-save-after-change nil
  "When non-nil, save bookmarks after Folio changes.
When nil, rely on `bookmark-save-flag' and Emacs shutdown."
  :type 'boolean)

(defconst folio--status-unread "unread"
  "Status string for unread entries.")

(defconst folio--status-read "read"
  "Status string for read entries.")

(defconst folio--status-archived "archived"
  "Status string for archived entries.")

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

(defvar-local folio--note-edit-id nil
  "Entry ID for the current note edit buffer.")

(defvar-local folio--note-edit-entry nil
  "Entry data for the current note edit buffer.")

(defvar-local folio--last-echo-title nil
  "Last entry title echoed in the folio list buffer.")

(defvar-local folio--list-entries nil
  "Cached entries for the folio list buffer.")

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
  "Face for folio types."
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

(defface folio-archived-face
  '((t :inherit shadow))
  "Face for archived entries."
  :group 'folio)

(defface folio-timestamp-face
  '((t :inherit font-lock-constant-face))
  "Face for folio added time."
  :group 'folio)

(defun folio--ensure-bookmarks-loaded ()
  "Ensure the Emacs bookmark database is loaded."
  (bookmark-maybe-load-default-file))

(defun folio--save-bookmarks ()
  "Save the Emacs bookmark database when configured."
  (when folio-save-after-change
    (let ((inhibit-message t))
      (bookmark-save))))

(defun folio--invalidate-cache ()
  "Invalidate cached folio entries."
  (setq folio--list-entries nil))

(defun folio--entry-status (entry)
  "Return the status string for ENTRY."
  (or (alist-get 'status entry) ""))

(defun folio--entry-unread-p (entry)
  "Return non-nil when ENTRY is unread."
  (string= (folio--entry-status entry) folio--status-unread))

(defun folio-bookmark-url-handler (bookmark)
  "Open a URL from BOOKMARK."
  (let* ((pair (folio--bookmark->name+record bookmark))
         (record (cdr-safe pair))
         (url (and record (alist-get 'url record))))
    (if url
        (funcall folio-url-open-function url)
      (message "Folio: no URL in bookmark"))))

(defun folio--bookmark-name-for-id (id)
  "Return bookmark name for Folio ID specified by ID."
  (car (seq-find (lambda (bm)
                   (string= id (alist-get 'folio-id (cdr bm))))
                 bookmark-alist)))

(defun folio--bookmark->name+record (bookmark)
  "Return (NAME . RECORD) for BOOKMARK, or nil."
  (let* ((name (cond
                ((stringp bookmark) bookmark)
                ((and (consp bookmark) (stringp (car bookmark))) (car bookmark))))
         (record (cond
                  ((and (consp bookmark)
                        (stringp (car bookmark))
                        (consp (cdr bookmark)))
                   (cdr bookmark))
                  ((and (consp bookmark) (alist-get 'url bookmark))
                   bookmark)
                  ((fboundp 'bookmark-get-bookmark-record)
                   (bookmark-get-bookmark-record (or name bookmark)))
                  (name (bookmark-get-bookmark name t)))))
    (when (and name record)
      (cons name (folio--unwrap-bookmark-record record)))))

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

(defun folio--bookmark-record->entry (name record)
  "Convert bookmark NAME and RECORD into a folio entry."
  (let* ((type (cond
                ((alist-get 'url record) "url")
                ((alist-get 'filename record) "file")
                ((alist-get 'handler record) "bookmark")
                (t (alist-get 'folio-type record))))
         (folio-id (alist-get 'folio-id record))
         (folio-p (and folio-id t))
         (added (or (alist-get 'folio-added record)
                    (when (and (alist-get 'last-modified record)
                               (fboundp 'bookmark-time-to-time))
                       (format-time-string "%Y-%m-%d %H:%M"
                                           (bookmark-time-to-time
                                            (alist-get 'last-modified record))))))
         (entry `((id . ,(or (alist-get 'folio-id record) name))
                  (folio-id . ,folio-id)
                  (bookmark . ,name)
                  (type . ,type)
                  (title . ,name)
                  (handler . ,(alist-get 'handler record))
                  (record . ,(copy-sequence record))
                  (tags . ,(alist-get 'folio-tags record))
                  (note . ,(alist-get 'annotation record))
                  (status . ,(when folio-p
                               (or (alist-get 'folio-status record) folio--status-unread)))
                  (added . ,added))))
    (pcase type
      ("url" (setf (alist-get 'url entry) (alist-get 'url record)))
      ("file"
       (setf (alist-get 'path entry) (alist-get 'filename record))
       (setf (alist-get 'line entry) (alist-get 'folio-line record)))
      ("bookmark"
       (setf (alist-get 'handler entry) (alist-get 'handler record))))
    entry))

(defun folio--bookmarks->db ()
  "Return a list of folio entries from the bookmark database."
  (let (entries)
    (dolist (bm bookmark-alist)
      (let ((name (car bm))
            (record (cdr bm)))
        (push (folio--bookmark-record->entry name record) entries)))
    entries))

(defun folio--refresh-db (&optional force)
  "Ensure bookmark data is loaded and cache is up to date.

When FORCE is non-nil, rebuild the cached entries list."
  (let ((force (if (null force) t force)))
    (folio--ensure-bookmarks-loaded)
    (folio--convert-bookmarks)
    (setq folio--loaded-p t)
    (when (or force (null folio--list-entries))
      (setq folio--list-entries (folio--bookmarks->db))))
  folio--list-entries)

(defun folio--entries ()
  "Return folio entries from the bookmark database, or nil if not loaded."
  (when (folio--ensure-loaded)
    folio--list-entries))

(defun folio--entries-for-list ()
  "Return entries for list rendering."
  (or folio--list-entries '()))

(defun folio--entry->bookmark-record (entry)
  "Convert ENTRY into a bookmark record."
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
                        (when handler
                          `((handler . ,handler)))))
                   (_ nil))))
    (when (member type '("url" "file" "bookmark"))
      (let ((id (alist-get 'id entry))
            (tags (alist-get 'tags entry))
            (note (alist-get 'note entry))
            (status (alist-get 'status entry))
            (added (alist-get 'added entry))
            (line (alist-get 'line entry)))
        (when id (push (cons 'folio-id (copy-sequence id)) record))
        (when tags (push (cons 'folio-tags (copy-sequence tags)) record))
        (unless (string-blank-p (or note ""))
          (push (cons 'annotation (copy-sequence note)) record))
        (when status (push (cons 'folio-status status) record))
        (when added (push (cons 'folio-added added) record))
        (when line (push (cons 'folio-line line) record))))
    record))

(defun folio--merge-record-allow-remove (base-record updated-record &rest allow-remove-keys)
  "Merge UPDATED-RECORD into BASE-RECORD, removing ALLOW-REMOVE-KEYS when nil."
  (let ((record (copy-sequence base-record)))
    (dolist (pair updated-record)
      (let ((key (car pair))
            (value (cdr pair)))
        (if (and (memq key allow-remove-keys) (null value))
            (setq record (assq-delete-all key record))
          (setf (alist-get key record) value))))
    record))

(defun folio--merge-record-if-missing (base-record updated-record)
  "Merge UPDATED-RECORD into BASE-RECORD only when keys are missing."
  (let ((record (copy-sequence base-record)))
    (dolist (pair updated-record)
      (unless (assoc (car pair) record)
        (push (cons (car pair) (cdr pair)) record)))
    record))

(defun folio--unwrap-bookmark-record (record)
  "Return bookmark RECORD without a leading name cell."
  (if (and (consp record) (stringp (car record)) (consp (cdr record)))
      (cdr record)
    record))

(defun folio--convert-bookmarks ()
  "Convert bookmarks to Folio entries, returning non-nil if any were updated."
  (let ((converted nil))
    (dolist (bm bookmark-alist)
      (let* ((record (folio--unwrap-bookmark-record (cdr bm))))
        (unless (alist-get 'folio-id record)
          (let* ((last-modified (alist-get 'last-modified record))
                 (added (when (and last-modified (fboundp 'bookmark-time-to-time))
                          (format-time-string "%Y-%m-%d %H:%M"
                                              (bookmark-time-to-time last-modified))))
                 (now (format-time-string "%Y-%m-%d %H:%M"))
                 (updates `((folio-id . ,(copy-sequence (folio--new-id)))
                            (folio-status . ,folio--status-unread)
                            (folio-added . ,(or added now)))))
            (setcdr bm (folio--merge-record-if-missing record updates))
            (setq converted t)))))
    converted))

(defun folio--store-entry-as-bookmark (entry &optional name)
  "Store ENTRY as a bookmark and return its bookmark NAME."
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
    (folio--invalidate-cache)
    (folio--save-bookmarks)
    bm-name))

(defun folio--ensure-loaded ()
  "Ensure the database is loaded."
  (when (or (not folio--loaded-p) (null folio--list-entries))
    (folio--refresh-db))
  folio--loaded-p)

(defun folio--refresh-list-buffer ()
  "Refresh the folio list buffer if it exists."
  (let ((buf (get-buffer "*Folio*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'folio-list-mode)
          (folio-list-refresh))))))

(defun folio--entry-at-point ()
  "Return (ID . ENTRY) at point."
  (let* ((id (tabulated-list-get-id))
         (entry (and id (folio--find-entry id))))
    (cond
     ((not entry)
      (message "Folio: no entry at point")
      nil)
     (t (cons id entry)))))

(cl-defmacro folio--with-entry-at-point ((id entry) &rest body)
  "Bind ID and ENTRY to the item at point, then run BODY."
  (declare (indent 1))
  `(let* ((pair (folio--entry-at-point))
          (,id (car-safe pair))
          (,entry (cdr-safe pair)))
     (when ,entry
       ,@body)))

(defun folio--store-entry-with-name (entry name &optional old-name)
  "Store ENTRY as bookmark NAME, removing OLD-NAME when it differs."
  (folio--ensure-bookmarks-loaded)
  (let* ((existing (bookmark-get-bookmark name t))
         (record (folio--entry->bookmark-record entry))
         (note (alist-get 'note entry))
         (note-blank (string-blank-p (or note "")))
         (has-folio (and existing
                         (assq 'folio (folio--unwrap-bookmark-record existing))))
         (record (if (and existing note-blank)
                     (cons (cons 'annotation nil) record)
                   record))
         (record (if has-folio
                     (cons (cons 'folio nil) record)
                   record))
         (merged (when (and record existing)
                   (folio--merge-record-allow-remove
                    (folio--unwrap-bookmark-record existing)
                    record
                    'annotation
                    'folio))))
    (when (and old-name (not (string= old-name name)))
      (bookmark-delete old-name t))
    (bookmark-store name (or merged record) nil)
    (folio--invalidate-cache)
    (folio--save-bookmarks)))

(defun folio--new-id ()
  "Generate a reasonably unique ID string."
  (format "%s-%06x"
          (format-time-string "%Y%m%d%H%M%S")
          (random #xFFFFFF)))

(defun folio--clean-tags (tags)
  "Normalize TAGS list by trimming, downcasing, dropping blanks, and deduplicating."
  (let* ((cleaned (mapcar (lambda (tag) (downcase (string-trim tag))) tags))
         (non-blank (seq-filter (lambda (tag) (not (string-blank-p tag))) cleaned))
         (unique (seq-uniq non-blank #'string=)))
    (sort unique #'string-lessp)))

(defun folio--read-tags (&optional initial-tags)
  "Read tags as a list of strings.
INITIAL-TAGS is a list of strings used as the initial input."
  (let* ((choices (folio--all-tags))
         (initial (when initial-tags (folio--format-tags initial-tags)))
         (tags (completing-read-multiple
                "Tags: "
                choices nil nil initial nil nil))
         (cleaned (folio--clean-tags tags)))
    (unless (null cleaned) cleaned)))

(defun folio--decode-octal-escapes (title)
  "Decode octal escapes like \\342\\200\\234 in TITLE."
  (when (string-match-p "\\\\[0-7]\\{3\\}" title)
    (let* ((octal-decoded
            (replace-regexp-in-string
             "\\\\([0-7]\\{3\\})"
             (lambda (match)
               (string (string-to-number (substring match 1) 8)))
             title t t))
           (raw-bytes (string-to-unibyte octal-decoded)))
      (condition-case nil
          (decode-coding-string raw-bytes 'utf-8)
        (error octal-decoded)))))

(defun folio--normalize-html-title (title)
  "Normalize TITLE parsed from HTML."
  (or (folio--decode-octal-escapes title)
      (condition-case nil
          (decode-coding-string (string-to-unibyte title) 'utf-8)
        (error title))))

(defun folio--current-url ()
  "Return a URL near point, or nil."
  (or (thing-at-point 'url t)
      (and (stringp (car kill-ring))
           (string-match-p "^https?://" (car kill-ring))
           (car kill-ring))))

(defun folio--normalize-url (url)
  "Normalize URL by trimming and ensuring a scheme."
  (let ((clean-url (when (stringp url) (string-trim url))))
    (when (and (stringp clean-url) (not (string-blank-p clean-url)))
      (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*://" clean-url)
          clean-url
        (concat "https://" clean-url)))))

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

(defun folio--guess-title-from-url-heuristic (url)
  "Guess a reasonable title from URL via parsing."
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

(defun folio--guess-title-from-url (url)
  "Guess a reasonable title from URL."
  (let* ((title nil)
         (fetch-url (folio--normalize-url url)))
    (when (and fetch-url (string-match-p "\\`https?://" fetch-url))
      (let ((buf (condition-case nil
                     (url-retrieve-synchronously fetch-url t t 10)
                   (error nil))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (goto-char (point-min))
            (when (search-forward "\n\n" nil t)
              (let ((case-fold-search t))
                (when (re-search-forward "<title[^>]*>\\(.*?\\)</title>" nil t)
                  (setq title (folio--normalize-html-title (match-string 1)))))))
          (kill-buffer buf))))
    (if (string-blank-p (or title ""))
        (folio--guess-title-from-url-heuristic (or fetch-url url))
      (string-trim (replace-regexp-in-string "[\t\n\r ]+" " " title)))))

(defun folio--find-entry (id)
  "Return entry with ID or nil."
  (seq-find (lambda (entry)
              (string= id (alist-get 'id entry)))
            (folio--entries)))

(defun folio--replace-entry (id new-entry)
  "Replace entry with ID by NEW-ENTRY in bookmarks."
  (folio--ensure-bookmarks-loaded)
  (let* ((name (or (alist-get 'bookmark new-entry)
                   (folio--bookmark-name-for-id id)))
         (record (folio--entry->bookmark-record new-entry)))
    (when (and name record)
      (folio--store-entry-with-name new-entry name))))

(defun folio--delete-entry (id)
  "Delete entry with ID."
  (folio--ensure-bookmarks-loaded)
  (let ((name (folio--bookmark-name-for-id id)))
    (when name
      (bookmark-delete name t)
      (folio--invalidate-cache)
      (folio--save-bookmarks))))

(defun folio--format-tags (tags)
  "Format TAGS list for display."
  (if tags
      (string-join tags ",")
    ""))

(defun folio--format-tags-clickable (tags row-face)
  "Format TAGS list with clickable text using ROW-FACE when non-nil."
  (when tags
    (mapconcat
     (lambda (tag)
       (let ((face (if row-face row-face 'folio-tags-face)))
         (propertize tag
                     'face face
                     'mouse-face 'highlight
                     'help-echo "Filter by this tag"
                     'keymap folio--tag-map
                     'folio-tag tag)))
     tags
     ",")))

(defun folio--all-tags ()
  "Return a sorted list of all tags in the database."
  (sort (delete-dups
         (apply #'append
                (mapcar (lambda (entry) (alist-get 'tags entry))
                        (folio--entries))))
        #'string-lessp))

(defun folio--matches-filter (entry)
  "Return non-nil if ENTRY matches current filters."
  (let ((tags (alist-get 'tags entry)))
    (if folio--filter-tags
        (seq-every-p (lambda (tag)
                       (member tag tags))
                     folio--filter-tags)
      t)))

(defun folio--entry-archived-p (entry)
  "Return non-nil if ENTRY is archived."
  (string= (folio--entry-status entry) folio--status-archived))

(defun folio--entry< (a b)
  "Compare entries A and B for list sorting."
  (let ((a-archived (folio--entry-archived-p a))
        (b-archived (folio--entry-archived-p b)))
    (cond
     ((and a-archived (not b-archived)) nil)
     ((and b-archived (not a-archived)) t)
     ((eq folio-list-sort-key 'title)
      (let ((ta (downcase (or (alist-get 'title a) "")))
            (tb (downcase (or (alist-get 'title b) ""))))
        (if (string= ta tb)
            (string< (or (alist-get 'added b) "")
                     (or (alist-get 'added a) ""))
          (string< ta tb))))
     (t
      (string< (or (alist-get 'added b) "")
               (or (alist-get 'added a) ""))))))

(defun folio--capped-column-width (accessor)
  "Return a width for column using ACCESSOR on each entry."
  (let ((max-len 0))
    (dolist (entry (seq-filter #'folio--matches-filter (folio--entries-for-list)))
      (setq max-len
            (max max-len (string-width (or (funcall accessor entry) "")))))
    (max 5 max-len)))

(defun folio--string-display-width (string)
  "Return the display width of STRING in columns."
  (if (and (fboundp 'string-pixel-width) (fboundp 'frame-char-width))
      (let* ((pixels (string-pixel-width string))
             (char-width (frame-char-width)))
        (if (> char-width 0)
            (ceiling (/ (float pixels) char-width))
          (string-width string)))
    (string-width string)))

(defun folio--tags-column-width ()
  "Return uncapped width for the tags column."
  (let ((max-len 0))
    (dolist (entry (seq-filter #'folio--matches-filter (folio--entries-for-list)))
      (setq max-len
            (max max-len
                 (string-width
                  (folio--format-tags (alist-get 'tags entry))))))
    (max 4 max-len)))

(defun folio--nerd-icons-available-p ()
  "Return non-nil when Nerd Icons can be used."
  (and folio-nerd-icons-enabled
       (require 'nerd-icons nil t)))

(defun folio--entry-icon (entry row-face)
  "Return a Nerd Icon string for ENTRY using ROW-FACE when non-nil."
  (when (folio--nerd-icons-available-p)
    ;; For files, try to pick an extension-aware icon; otherwise fall back.
    (let* ((type (alist-get 'type entry))
           (type-face (pcase type
                        ("file" 'folio-type-file-face)
                        (_ 'folio-type-url-face)))
           (display-face (or row-face type-face)))
      (pcase type
        ("file"
         (let ((path (alist-get 'path entry)))
           (if (and path (fboundp 'nerd-icons-icon-for-file))
               (nerd-icons-icon-for-file
                (file-name-nondirectory path)
                :face display-face)
             (nerd-icons-octicon "nf-oct-file" :face display-face))))
        ("bookmark" (nerd-icons-octicon "nf-oct-bookmark" :face display-face))
        (_ (nerd-icons-octicon "nf-oct-link" :face display-face))))))

(defun folio--title-column-width ()
  "Return the title column width, including icon padding when enabled."
  (let* ((use-icons (folio--nerd-icons-available-p))
         (entries (seq-filter #'folio--matches-filter (folio--entries-for-list)))
         (max-len 0))
    ;; Measure the rendered title prefix (icon + space) to keep columns aligned.
    (dolist (entry entries)
      (let* ((title (or (alist-get 'title entry) ""))
             (prefix (and use-icons
                          (let ((icon (folio--entry-icon entry nil)))
                            (and icon (concat icon " ")))))
             (text (if prefix (concat prefix title) title))
             (width (if use-icons
                        (folio--string-display-width text)
                      (string-width text))))
        (setq max-len (max max-len width))))
    (max 5 max-len)))

(defun folio--tabulated-list-format ()
  "Return the tabulated list format for folio entries."
  (if (folio--nerd-icons-available-p)
      ;; When icons are shown in titles, drop the separate Type column.
      (vector
       (list "Added" 16 t)
       (list "Title" (folio--title-column-width) t)
       (list "Tags" (folio--tags-column-width) t)
       (list "Unread" 6 t)
       (list "Note" 4 t)
       (list "Location" 36 t))
    (vector
     (list "Added" 16 t)
     (list "Type" (folio--capped-column-width (lambda (e) (alist-get 'type e))) t)
     (list "Title" (folio--title-column-width) t)
     (list "Tags" (folio--tags-column-width) t)
     (list "Unread" 6 t)
     (list "Note" 4 t)
     (list "Location" 36 t))))

(defun folio--entry->row (entry)
  "Convert ENTRY to a `tabulated-list' row."
  (let* ((archived (folio--entry-archived-p entry))
         (row-face (when archived 'folio-archived-face))
         (location (or (alist-get 'url entry)
                       (alist-get 'path entry)
                       ""))
         (unread (folio--entry-unread-p entry))
         (title-face (if row-face row-face 'folio-title-face))
         (icon (folio--entry-icon entry row-face))
         (title-text (or (alist-get 'title entry) ""))
         ;; Keep the title text face even when the icon has its own face.
         (title (concat (when icon (concat icon " "))
                        (propertize title-text 'face title-face)))
         (type-text (or (alist-get 'type entry) ""))
         (type-face (cond
                     (row-face row-face)
                     ((string= type-text "file") 'folio-type-file-face)
                     (t 'folio-type-url-face)))
         (type (propertize type-text 'face type-face))
         (location (propertize (truncate-string-to-width location 36 nil nil "...")
                               'face (if row-face row-face 'folio-location-face)
                               'mouse-face 'highlight
                               'help-echo "Open this entry"
                               'keymap folio--location-map))
         (tags (or (folio--format-tags-clickable (alist-get 'tags entry) row-face)
                   ""))
         (unread-flag (if unread
                          (propertize "*"
                                      'face (if row-face
                                                `(:inherit (,row-face folio-unread-face))
                                              'folio-unread-face))
                        ""))
         (note-text (alist-get 'note entry))
         (note (if (string-blank-p (or note-text ""))
                   ""
                 (propertize "+"
                             'face (if row-face
                                       `(:inherit (,row-face folio-note-face))
                                     'folio-note-face))))
         (added (propertize (or (alist-get 'added entry) "")
                            'face (if row-face row-face 'folio-timestamp-face))))
    (add-text-properties 0 (length title)
                         (list 'mouse-face 'highlight
                               'help-echo "Open this entry"
                               'keymap folio--location-map)
                         title)
    (list (alist-get 'id entry)
          (if (folio--nerd-icons-available-p)
              (vector added title tags unread-flag note location)
            (vector added type title tags unread-flag note location)))))

(defun folio-list-refresh ()
  "Refresh the folio list buffer."
  (interactive)
  (folio--refresh-db)
  (setq tabulated-list-format (folio--tabulated-list-format))
  (tabulated-list-init-header)
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-entries
        (mapcar #'folio--entry->row
                (seq-sort #'folio--entry<
                          (seq-filter #'folio--matches-filter
                                      (folio--entries-for-list)))))
  (tabulated-list-print t))

(defun folio-list-filter-tag-at-point (event)
  "Filter list by the tag at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (let ((tag (get-text-property (point) 'folio-tag)))
    (if tag
        (progn
          (setq folio--filter-tags (list tag))
          (folio-list-refresh))
      (message "Folio: no tag at point"))))

(defun folio--commit-entry (id entry)
  "Persist ENTRY for ID and refresh the list."
  (folio--replace-entry id entry)
  (folio-list-refresh))

(defun folio--refresh-keep-position ()
  "Refresh the folio list and keep point near its previous position."
  (let ((line (line-number-at-pos))
        (column (current-column))
        (start (window-start)))
    (folio-list-refresh)
    (goto-char (point-min))
    (forward-line (1- (min line (line-number-at-pos (point-max)))))
    (move-to-column column)
    (set-window-start (selected-window) start)))

;;;###autoload
(defun folio-add-url (url)
  "Add a URL bookmark.

URL is read from the minibuffer with a helpful default."
  (interactive
   (list (read-string "URL: " (folio--current-url))))
  (folio--ensure-loaded)
  (let* ((url (or (folio--normalize-url url) url))
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
  (folio--ensure-loaded)
  (let* ((path (expand-file-name file))
         (line (when (and (buffer-file-name)
                          (string= (expand-file-name (buffer-file-name)) path))
                 (line-number-at-pos)))
         (title-default (file-name-nondirectory (directory-file-name path)))
         (entry (folio--capture-entry
                 "file"
                 title-default
                 (cons 'path path)
                 (cons 'line line))))
    (folio--store-entry-as-bookmark entry)
    (folio--refresh-list-buffer)
    (message "Folio: saved file")))

(defun folio-list-open ()
  "Open the entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (let ((status (folio--entry-status entry)))
      (when (and (not (string= status folio--status-archived))
                 (not (string= status folio--status-read)))
        (setf (alist-get 'status entry) folio--status-read)
        (folio--commit-entry id entry)))
    (let ((name (alist-get 'bookmark entry)))
      (if name
          (bookmark-jump name)
        (message "Folio: no bookmark name for entry")))))

(defun folio-list-toggle-read ()
  "Toggle read/unread status for entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (let* ((status (folio--entry-status entry))
           (new-status (if (string= status folio--status-read)
                           folio--status-unread
                         folio--status-read)))
      (setf (alist-get 'status entry) new-status)
      (folio--commit-entry id entry))))

(defun folio-list-archive ()
  "Archive entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (setf (alist-get 'status entry) folio--status-archived)
    (folio--commit-entry id entry)))

(defun folio-list-unarchive ()
  "Unarchive entry at point, setting status to unread."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (setf (alist-get 'status entry) folio--status-unread)
    (folio--commit-entry id entry)))

(defun folio-list-edit-tags ()
  "Edit tags for entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (let ((tags (folio--read-tags (alist-get 'tags entry))))
      (setf (alist-get 'tags entry) tags)
      (folio--commit-entry id entry))))

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
    (folio--commit-entry id entry)
    (let ((buf (get-buffer "*Folio*")))
      (when (buffer-live-p buf)
        (let ((win (get-buffer-window buf t)))
          (with-current-buffer buf
            (when (derived-mode-p 'folio-list-mode)
              (if win
                  (with-selected-window win
                    (folio--refresh-keep-position))
                (folio-list-refresh)))))))
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

(defun folio-list-edit-location ()
  "Edit URL or file path for entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (pcase (alist-get 'type entry)
      ("url"
       (let* ((current (or (alist-get 'url entry) ""))
              (url (read-string "URL: " current)))
         (setf (alist-get 'url entry) url)
         (folio--commit-entry id entry)))
      ("file"
       (let* ((current (or (alist-get 'path entry) ""))
              (path (read-file-name "File: " nil current t)))
         (setf (alist-get 'path entry) (expand-file-name path))
         (folio--commit-entry id entry)))
      (_ (message "Folio: unknown entry type")))))

(defun folio-list-delete ()
  "Delete entry at point."
  (interactive)
  (folio--with-entry-at-point (id entry)
    (let ((name (or (alist-get 'bookmark entry)
                    (folio--bookmark-name-for-id id))))
      (cond
       ((not name)
        (message "Folio: no bookmark name for entry"))
       ((y-or-n-p "Delete this entry? ")
        (folio--delete-entry id)
        (folio--refresh-keep-position))
       (t (message "Folio: delete canceled"))))))

(defvar folio-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'folio-list-open)
    (define-key map (kbd "o") #'folio-list-open)
    (define-key map (kbd "/") #'folio-list-filter-tags)
    (define-key map (kbd "*") #'folio-list-toggle-read)
    (define-key map (kbd "a") #'folio-list-archive)
    (define-key map (kbd "u") #'folio-list-unarchive)
    (define-key map (kbd "e l") #'folio-list-edit-title)
    (define-key map (kbd "+") #'folio-list-edit-note)
    (define-key map (kbd "e u") #'folio-list-edit-location)
    (define-key map (kbd "e t") #'folio-list-edit-tags)
    (define-key map (kbd "d") #'folio-list-delete)
    (define-key map (kbd "g") #'folio-list-refresh)
    (define-key map (kbd "; l") #'folio-list-sort-by-title)
    (define-key map (kbd "; t") #'folio-list-sort-by-time)
    (define-key map (kbd "-") #'folio-add-url)
    (define-key map (kbd "=") #'folio-add-file)
    map)
  "Keymap for `folio-list-mode'.")

(defun folio--echo-entry-title ()
  "Echo the current entry title in the echo area."
  (when (derived-mode-p 'folio-list-mode)
    (let* ((id (tabulated-list-get-id))
           (entry (and id (folio--find-entry id)))
           (title (and entry (alist-get 'title entry))))
      (when (and title (not (string= title folio--last-echo-title)))
        (setq folio--last-echo-title title)
        (message "%s" title)))))

(define-derived-mode folio-list-mode tabulated-list-mode "Folio"
  "Major mode for listing folio entries."
  (setq tabulated-list-format (folio--tabulated-list-format))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'folio-list-refresh nil t)
  (add-hook 'post-command-hook #'folio--echo-entry-title nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun folio-list ()
  "Show the folio list buffer."
  (interactive)
  (folio--ensure-loaded)
  (let ((buf (get-buffer-create "*Folio*")))
    (with-current-buffer buf
      (folio-list-mode)
      (folio-list-refresh))
    (pop-to-buffer buf)))

(defun folio-list-filter-tags (tags)
  "Filter list by TAGS list."
  (interactive
   (list
    (let ((choices (folio--all-tags)))
      (completing-read-multiple
       "Tags: "
       choices nil t nil nil nil))))
  (setq folio--filter-tags tags)
  (folio-list-refresh))

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

;;;###autoload
(defun folio-bookmark-set (name)
  "Create or update a Folio bookmark at point.

NAME is the bookmark name, like `bookmark-set'."
  (interactive (list (bookmark-completing-read "Set Folio bookmark: ")))
  (folio--ensure-bookmarks-loaded)
  (bookmark-set name)
  (let ((record (bookmark-get-bookmark name t)))
    (when record
      (let* ((record (folio--unwrap-bookmark-record record))
             (now (format-time-string "%Y-%m-%d %H:%M"))
             (updates `((folio-id . ,(copy-sequence (folio--new-id)))
                        (folio-status . ,folio--status-unread)
                        (folio-added . ,now)))
             (merged (folio--merge-record-if-missing record updates)))
        (bookmark-store name merged nil)
        (folio--invalidate-cache)
        (folio--save-bookmarks)
        (folio--refresh-list-buffer)
        (message "Folio: saved bookmark")))))

(defun folio--tabulated-list-sort-advice (orig &rest args)
  "Handle tabulated-list header clicks in `folio-list-mode'.

ORIG is the original function and ARGS are its arguments."
  (if (derived-mode-p 'folio-list-mode)
      (progn
        (pcase (car-safe tabulated-list-sort-key)
          ("Title" (setq folio-list-sort-key 'title))
          ("Added" (setq folio-list-sort-key 'added)))
        (setq tabulated-list-sort-key nil)
        (folio-list-refresh))
    (apply orig args)))

(unless (advice-member-p #'folio--tabulated-list-sort-advice 'tabulated-list-sort)
  (advice-add 'tabulated-list-sort :around #'folio--tabulated-list-sort-advice))

(defun folio--bookmark-mark-read (bookmark)
  "Mark BOOKMARK as read when it belongs to Folio."
  (let* ((pair (folio--bookmark->name+record bookmark))
         (name (car-safe pair))
         (record (cdr-safe pair)))
    (when record
      (let* ((status (or (alist-get 'folio-status record) ""))
             (folio-id (alist-get 'folio-id record))
             (now (format-time-string "%Y-%m-%d %H:%M")))
        (unless folio-id
          (setf (alist-get 'folio-id record) (copy-sequence (folio--new-id)))
          (setf (alist-get 'folio-status record) folio--status-unread)
          (setf (alist-get 'folio-added record) now)
          (setq status folio--status-unread))
        (unless (or (string= status folio--status-archived)
                    (string= status folio--status-read))
          (setf (alist-get 'folio-status record) folio--status-read)
          (bookmark-store name record nil)
          (folio--save-bookmarks)
          (folio--refresh-list-buffer))))))

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
  (let* ((external-p (folio--bookmark-external-p bookmark)))
    ;; Bind `bookmark-fringe-mark' dynamically so only this jump is affected.
    (let ((bookmark-fringe-mark (if external-p nil bookmark-fringe-mark)))
      (let ((result (apply orig bookmark args)))
        (folio--bookmark-mark-read bookmark)
        result))))

(unless (advice-member-p #'folio--bookmark-jump-advice 'bookmark-jump)
  (advice-add 'bookmark-jump :around #'folio--bookmark-jump-advice))

(add-hook 'bookmark-after-jump-hook #'folio--bookmark-after-jump)

(defun folio--bookmark-change-advice (&rest _args)
  "Invalidate Folio cache when bookmarks change.

_ARGS are ignored."
  (folio--invalidate-cache))

(unless (advice-member-p #'folio--bookmark-change-advice 'bookmark-store)
  (advice-add 'bookmark-store :after #'folio--bookmark-change-advice))

(unless (advice-member-p #'folio--bookmark-change-advice 'bookmark-delete)
  (advice-add 'bookmark-delete :after #'folio--bookmark-change-advice))

(when (fboundp 'bookmark-load)
  (unless (advice-member-p #'folio--bookmark-change-advice 'bookmark-load)
    (advice-add 'bookmark-load :after #'folio--bookmark-change-advice)))

(provide 'folio)
;;; folio.el ends here
