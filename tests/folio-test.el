;;; folio-test.el --- ERT tests for folio.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -L . -L tests -l folio -l tests/folio-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'folio)

;;; ─── folio--clean-tags ───────────────────────────────────────────────────────

(ert-deftest folio--clean-tags/basic-sort ()
  (should (equal (folio--clean-tags '("Foo" "bar" "baz")) '("bar" "baz" "foo"))))

(ert-deftest folio--clean-tags/downcase ()
  (should (equal (folio--clean-tags '("FOO" "Foo" "foo")) '("foo"))))

(ert-deftest folio--clean-tags/trim-whitespace ()
  (should (equal (folio--clean-tags '("  foo  " " bar")) '("bar" "foo"))))

(ert-deftest folio--clean-tags/drop-blank ()
  (should (equal (folio--clean-tags '("" "  " "foo")) '("foo"))))

(ert-deftest folio--clean-tags/empty-input ()
  (should (null (folio--clean-tags '()))))

(ert-deftest folio--clean-tags/dedup-case-insensitive ()
  "After downcasing, duplicates are removed."
  (should (equal (folio--clean-tags '("Emacs" "emacs" "EMACS")) '("emacs"))))

;;; ─── folio--normalize-url ────────────────────────────────────────────────────

(ert-deftest folio--normalize-url/with-scheme ()
  (should (equal (folio--normalize-url "https://example.com") "https://example.com")))

(ert-deftest folio--normalize-url/adds-https ()
  (should (equal (folio--normalize-url "example.com") "https://example.com")))

(ert-deftest folio--normalize-url/trims-whitespace ()
  (should (equal (folio--normalize-url "  https://example.com  ") "https://example.com")))

(ert-deftest folio--normalize-url/blank-returns-nil ()
  (should (null (folio--normalize-url "   "))))

(ert-deftest folio--normalize-url/nil-returns-nil ()
  (should (null (folio--normalize-url nil))))

(ert-deftest folio--normalize-url/http-scheme-preserved ()
  (should (equal (folio--normalize-url "http://example.com") "http://example.com")))

;;; ─── folio--guess-title-from-url ─────────────────────────────────────────────

(ert-deftest folio--guess-title/path-segment-humanized ()
  (should (equal (folio--guess-title-from-url
                  "https://example.com/my-cool-post")
                 "my cool post")))

(ert-deftest folio--guess-title/host-fallback-on-root ()
  (should (equal (folio--guess-title-from-url "https://example.com/")
                 "example")))

(ert-deftest folio--guess-title/strips-www ()
  (should (equal (folio--guess-title-from-url "https://www.example.com/")
                 "example")))

(ert-deftest folio--guess-title/strips-tld ()
  (should (equal (folio--guess-title-from-url "https://example.org/")
                 "example")))

(ert-deftest folio--guess-title/index-page-falls-back-to-host ()
  (should (equal (folio--guess-title-from-url "https://example.com/index.html")
                 "example")))

(ert-deftest folio--guess-title/strips-file-extension ()
  (should (equal (folio--guess-title-from-url
                  "https://docs.rust-lang.org/book/ch01.html")
                 "ch01")))

(ert-deftest folio--guess-title/underscore-to-space ()
  (should (equal (folio--guess-title-from-url
                  "https://example.com/some_article_name")
                 "some article name")))

(ert-deftest folio--guess-title/multi-level-tld-stripped ()
  "A .co.uk style domain: only the last known TLD component is stripped."
  (let ((result (folio--guess-title-from-url "https://subdomain.example.co.uk/")))
    (should-not (string-match-p "\\.uk\\'" result))))

;;; ─── folio--unique-bookmark-name ─────────────────────────────────────────────

(ert-deftest folio--unique-bookmark-name/no-conflict ()
  (let ((bookmark-alist nil))
    (should (equal (folio--unique-bookmark-name "My Bookmark") "My Bookmark"))))

(ert-deftest folio--unique-bookmark-name/one-conflict ()
  (let ((bookmark-alist '(("My Bookmark" . ((filename . "/foo"))))))
    (should (equal (folio--unique-bookmark-name "My Bookmark") "My Bookmark (2)"))))

(ert-deftest folio--unique-bookmark-name/two-conflicts ()
  (let ((bookmark-alist '(("My Bookmark" . ((filename . "/foo")))
                          ("My Bookmark (2)" . ((filename . "/bar"))))))
    (should (equal (folio--unique-bookmark-name "My Bookmark") "My Bookmark (3)"))))

(ert-deftest folio--unique-bookmark-name/existing-name-allowed ()
  "EXISTING-NAME matching BASE should not trigger a suffix."
  (let ((bookmark-alist '(("My Bookmark" . ((filename . "/foo"))))))
    (should (equal (folio--unique-bookmark-name "My Bookmark" "My Bookmark")
                   "My Bookmark"))))

;;; ─── folio--merge-record-allow-remove ────────────────────────────────────────

(ert-deftest folio--merge-record-allow-remove/adds-new-key ()
  (let ((result (folio--merge-record-allow-remove '((a . 1)) '((b . 2)))))
    (should (equal (alist-get 'b result) 2))))

(ert-deftest folio--merge-record-allow-remove/updates-existing-key ()
  (let ((result (folio--merge-record-allow-remove '((a . 1) (b . 2)) '((b . 99)))))
    (should (equal (alist-get 'b result) 99))))

(ert-deftest folio--merge-record-allow-remove/removes-key-when-in-allow-list ()
  (let ((result (folio--merge-record-allow-remove '((a . 1) (b . 2))
                                                  '((b . nil))
                                                  'b)))
    (should-not (assq 'b result))))

(ert-deftest folio--merge-record-allow-remove/sets-nil-when-not-in-allow-list ()
  "A nil value for a key NOT in allow-remove-keys sets it to nil, not removes it."
  (let ((result (folio--merge-record-allow-remove '((a . 1) (b . 2))
                                                  '((b . nil)))))
    (should (assq 'b result))
    (should (null (alist-get 'b result)))))

(ert-deftest folio--merge-record-allow-remove/does-not-modify-base ()
  "The base-record must not be mutated."
  (let* ((base (list (cons 'a 1) (cons 'b 2)))
         (base-copy (copy-sequence base)))
    (folio--merge-record-allow-remove base '((b . 99)))
    (should (equal base base-copy))))

;;; ─── folio--merge-record-if-missing ─────────────────────────────────────────

(ert-deftest folio--merge-record-if-missing/adds-absent-key ()
  (let ((result (folio--merge-record-if-missing '((a . 1)) '((b . 2)))))
    (should (equal (alist-get 'b result) 2))))

(ert-deftest folio--merge-record-if-missing/does-not-overwrite ()
  (let ((result (folio--merge-record-if-missing '((a . 1)) '((a . 99)))))
    (should (equal (alist-get 'a result) 1))))

(ert-deftest folio--merge-record-if-missing/does-not-modify-base ()
  (let* ((base (list (cons 'a 1)))
         (base-copy (copy-sequence base)))
    (folio--merge-record-if-missing base '((a . 99)))
    (should (equal base base-copy))))

;;; ─── folio--entry< ───────────────────────────────────────────────────────────

(ert-deftest folio--entry</sort-by-time-newest-first ()
  (let ((folio-list-sort-key 'added)
        (older `((status . ,folio--status-unread) (added . "2024-01-01 12:00")))
        (newer `((status . ,folio--status-unread) (added . "2025-01-01 12:00"))))
    (should     (folio--entry< newer older))
    (should-not (folio--entry< older newer))))

(ert-deftest folio--entry</sort-by-title-alphabetical ()
  (let ((folio-list-sort-key 'title)
        (a `((status . ,folio--status-unread) (title . "Apple")  (added . "2025-01-01 12:00")))
        (b `((status . ,folio--status-unread) (title . "Banana") (added . "2025-01-01 12:00"))))
    (should     (folio--entry< a b))
    (should-not (folio--entry< b a))))

(ert-deftest folio--entry</sort-by-title-case-insensitive ()
  (let ((folio-list-sort-key 'title)
        (lower `((status . ,folio--status-unread) (title . "apple")  (added . "2025-01-01 12:00")))
        (upper `((status . ,folio--status-unread) (title . "Banana") (added . "2025-01-01 12:00"))))
    (should (folio--entry< lower upper))))

(ert-deftest folio--entry</title-tie-broken-by-time ()
  "Equal titles fall back to newest-first."
  (let ((folio-list-sort-key 'title)
        (a `((status . ,folio--status-unread) (title . "Same") (added . "2025-06-01 12:00")))
        (b `((status . ,folio--status-unread) (title . "Same") (added . "2024-01-01 12:00"))))
    (should (folio--entry< a b))))

;;; ─── folio--bookmark-record->entry round-trip ───────────────────────────────

(ert-deftest folio--url-entry-roundtrip ()
  "A URL entry survives a record conversion round-trip."
  (let* ((entry `((id . "test-id-001")
                  (type . "url")
                  (title . "Example")
                  (url . "https://example.com")
                  (tags . ("elisp" "emacs"))
                  (note . "A note")
                  (status . ,folio--status-unread)
                  (added . "2026-01-01 12:00")))
         (record  (folio--entry->bookmark-record entry))
         (rebuilt (folio--bookmark-record->entry "Example" record)))
    (should (equal (alist-get 'url    rebuilt) "https://example.com"))
    (should (equal (alist-get 'tags   rebuilt) '("elisp" "emacs")))
    (should (equal (alist-get 'note   rebuilt) "A note"))
    (should (equal (alist-get 'status rebuilt) folio--status-unread))))

(ert-deftest folio--file-entry-roundtrip ()
  "A file entry survives a record conversion round-trip."
  (let* ((entry `((id . "test-id-002")
                  (type . "file")
                  (title . "My file")
                  (path . "/home/user/notes.org")
                  (tags . ("org"))
                  (note . nil)
                  (status . ,folio--status-read)
                  (added . "2026-02-01 10:00")))
         (record  (folio--entry->bookmark-record entry))
         (rebuilt (folio--bookmark-record->entry "My file" record)))
    (should (equal (alist-get 'path   rebuilt) "/home/user/notes.org"))
    (should (equal (alist-get 'status rebuilt) folio--status-read))))

(ert-deftest folio--bookmarks->db/accepts-list-form-full-record ()
  "Bookmark conversion accepts the list-form full record used by Emacs."
  (let ((bookmark-alist
         '(("Example" ((url . "https://example.com")
                       (handler . folio-bookmark-url-handler))))))
    (let ((entry (car (folio--bookmarks->db))))
      (should (equal (alist-get 'bookmark entry) "Example"))
      (should (equal (alist-get 'url entry) "https://example.com")))))

;;; ─── Non-folio bookmark display (lazy adoption) ──────────────────────────────

(ert-deftest folio--non-folio-bookmark-displays-as-unread ()
  "A bookmark without folio-id still renders with status=unread."
  (let* ((record '((filename . "/some/file")))
         (entry (folio--bookmark-record->entry "Plain BM" record)))
    (should (equal (alist-get 'status entry) folio--status-unread))
    (should (null (alist-get 'folio-id entry)))
    (should (equal (alist-get 'id entry) "Plain BM"))))

(ert-deftest folio--non-folio-bookmark-has-no-tags-or-note ()
  "A bare bookmark has no tags and no note, and renders without errors."
  (let* ((record '((filename . "/some/file")))
         (entry (folio--bookmark-record->entry "Plain BM" record)))
    (should (null (alist-get 'tags entry)))
    (should (null (alist-get 'note entry)))))

;;; ─── Tag-clear bug regression ────────────────────────────────────────────────

(ert-deftest folio--entry->bookmark-record/always-includes-folio-tags ()
  "folio-tags must appear in the record even when tags is nil, so the merge
can remove an existing folio-tags key."
  (let* ((entry `((id . "test-id-003")
                  (type . "url")
                  (title . "Test")
                  (url . "https://example.com")
                  (tags . nil)
                  (status . ,folio--status-unread)
                  (added . "2026-01-01 12:00")))
         (record (folio--entry->bookmark-record entry)))
    (should (assq 'folio-tags record))
    (should (null (alist-get 'folio-tags record)))))

(ert-deftest folio--merge-removes-folio-tags-when-nil ()
  "Merging with nil folio-tags and folio-tags in allow-remove-keys removes the key."
  (let* ((base   '((folio-id . "abc") (folio-tags . ("old-tag"))))
         (update '((folio-tags . nil)))
         (result (folio--merge-record-allow-remove base update 'folio-tags)))
    (should-not (assq 'folio-tags result))))

(ert-deftest folio--clear-tags-full-merge ()
  "End-to-end: building an update record from a nil-tags entry and merging it
into an existing record with tags removes the tags key entirely."
  (let* ((existing-record '((folio-id . "abc")
                            (folio-tags . ("old-tag" "another"))
                            (folio-status . "unread")
                            (url . "https://example.com")
                            (handler . folio-bookmark-url-handler)))
         (entry `((id . "abc")
                  (type . "url")
                  (title . "Test")
                  (url . "https://example.com")
                  (tags . nil)
                  (note . nil)
                  (status . ,folio--status-unread)
                  (added . "2026-01-01 12:00")))
         (update-record (folio--entry->bookmark-record entry))
         (merged (folio--merge-record-allow-remove
                  existing-record
                  update-record
                  'annotation
                  'folio-tags)))
    (should-not (assq 'folio-tags merged))))

;;; ─── folio--entry-read-p / predicates ───────────────────────────────────────

(ert-deftest folio--entry-read-p/read ()
  (should (folio--entry-read-p `((status . ,folio--status-read)))))

(ert-deftest folio--entry-read-p/unread ()
  (should-not (folio--entry-read-p `((status . ,folio--status-unread)))))

(ert-deftest folio--entry-unread-p/unread ()
  (should (folio--entry-unread-p `((status . ,folio--status-unread)))))

(ert-deftest folio--entry-unread-p/read ()
  (should-not (folio--entry-unread-p `((status . ,folio--status-read)))))

;;; ─── folio--type-letter ──────────────────────────────────────────────────────

(ert-deftest folio--type-letter/url ()
  "URL entries produce \"U\" with url face."
  (let ((entry '((type . "url"))))
    (should (equal (substring-no-properties (folio--type-letter entry)) "U"))
    (should (eq (get-text-property 0 'face (folio--type-letter entry))
                'folio-type-url-face))))

(ert-deftest folio--type-letter/file ()
  "File entries produce \"F\" with file face."
  (let ((entry '((type . "file"))))
    (should (equal (substring-no-properties (folio--type-letter entry)) "F"))
    (should (eq (get-text-property 0 'face (folio--type-letter entry))
                'folio-type-file-face))))

(ert-deftest folio--type-letter/bookmark ()
  "Bookmark entries produce \"B\" with url face."
  (let ((entry '((type . "bookmark"))))
    (should (equal (substring-no-properties (folio--type-letter entry)) "B"))
    (should (eq (get-text-property 0 'face (folio--type-letter entry))
                'folio-type-url-face))))

(ert-deftest folio--type-letter/unknown-type ()
  "Unknown or missing type falls back to \"U\"."
  (should (equal (substring-no-properties (folio--type-letter '((type . "other")))) "U"))
  (should (equal (substring-no-properties (folio--type-letter '())) "U")))

;;; ─── folio--entry->row ──────────────────────────────────────────────────────

(ert-deftest folio--entry->row/title-has-type-prefix ()
  "The title cell starts with the type indicator followed by a space."
  (cl-letf (((symbol-function 'folio--nerd-icons-available-p) #'ignore))
    (let* ((entry `((id . "t1") (type . "url") (title . "Example")
                    (url . "https://example.com") (tags . nil) (note . nil)
                    (status . ,folio--status-unread) (added . "2026-01-01 12:00")))
           (row (folio--entry->row entry))
           (title-cell (aref (cadr row) 1)))
      (should (string-prefix-p "U " (substring-no-properties title-cell)))
      (should (string-match-p "Example" title-cell)))))

;;; ─── folio--list-format ─────────────────────────────────────────────────────

(ert-deftest folio--list-format/column-count ()
  "Format vector has 6 columns: Added, Title, Tags, Unread, Note, Location."
  (let ((fmt (folio--list-format 20 10)))
    (should (= (length fmt) 6))
    (should (equal (car (aref fmt 0)) "Added"))
    (should (equal (car (aref fmt 1)) "Title"))
    (should (equal (car (aref fmt 2)) "Tags"))))

(ert-deftest folio--list-format/widths-applied ()
  "Dynamic widths are applied to Title and Tags columns."
  (let ((fmt (folio--list-format 42 15)))
    (should (= (cadr (aref fmt 1)) 42))
    (should (= (cadr (aref fmt 2)) 15))))

;;; ─── folio--all-tags ────────────────────────────────────────────────────────

(ert-deftest folio--all-tags/collects-and-sorts ()
  "Tags from all entries are collected, deduplicated, and sorted."
  (let ((folio--list-entries
         `(((tags . ("beta" "alpha")))
           ((tags . ("gamma" "alpha"))))))
    (cl-letf (((symbol-function 'folio--ensure-bookmarks-loaded)
               (lambda () nil)))
      (should (equal (folio--all-tags) '("alpha" "beta" "gamma"))))))

(ert-deftest folio--all-tags/empty-db ()
  "Returns nil when no entries have tags."
  (let ((folio--list-entries '(((tags . nil)) ((tags . nil)))))
    (cl-letf (((symbol-function 'folio--ensure-bookmarks-loaded)
               (lambda () nil)))
      (should (null (folio--all-tags))))))

(ert-deftest folio--all-tags/does-not-mutate-entry-tags ()
  "mapcan with copy-sequence must not mutate the original tag lists."
  (let* ((entry1 (list (cons 'tags (list "a" "b"))))
         (entry2 (list (cons 'tags (list "c"))))
         (folio--list-entries (list entry1 entry2)))
    (cl-letf (((symbol-function 'folio--ensure-bookmarks-loaded)
               (lambda () nil)))
      (folio--all-tags)
      (should (equal (alist-get 'tags entry1) '("a" "b")))
      (should (equal (alist-get 'tags entry2) '("c"))))))

;;; ─── folio--invalidate-cache / cache state ──────────────────────────────────

(ert-deftest folio--entries/reloads-when-cold ()
  "folio--entries rebuilds a cold cache, then reuses it."
  (let ((folio--list-entries folio--cold-cache)
        (call-count 0))
    (cl-letf (((symbol-function 'folio--ensure-bookmarks-loaded)
               (lambda () nil))
              ((symbol-function 'folio--bookmarks->db)
               (lambda ()
                 (cl-incf call-count)
                 '(dummy))))
      (folio--entries)
      (should (= call-count 1))
      (folio--entries)
      (should (= call-count 1)))))

(ert-deftest folio--entries/caches-empty-database ()
  "An empty bookmark database is rebuilt only once while the cache is warm."
  (let ((folio--list-entries folio--cold-cache)
        (call-count 0))
    (cl-letf (((symbol-function 'folio--ensure-bookmarks-loaded) #'ignore)
              ((symbol-function 'folio--bookmarks->db)
               (lambda ()
                 (cl-incf call-count)
                 nil)))
      (should-not (folio--entries))
      (should-not (folio--entries))
      (should (= call-count 1)))))

(ert-deftest folio--delete-entry/deletes-plain-bookmark ()
  "A bookmark without a folio-id can be deleted by its fallback ID."
  (let ((bookmark-alist '(("Plain bookmark" . ((filename . "/tmp/plain")))))
        deleted)
    (cl-letf (((symbol-function 'folio--ensure-bookmarks-loaded) #'ignore)
              ((symbol-function 'bookmark-delete)
               (lambda (name &optional _batch)
                 (setq deleted name))))
      (folio--delete-entry "Plain bookmark")
      (should (equal deleted "Plain bookmark")))))

(ert-deftest folio--bookmark-name-for-id/prefers-folio-id-over-plain-name ()
  "A folio-id match wins over a conflicting plain bookmark name."
  (let ((bookmark-alist
         '(("shared-id" . ((filename . "/tmp/plain")))
           ("Folio entry" . ((folio-id . "shared-id")
                             (filename . "/tmp/folio"))))))
    (should (equal (folio--bookmark-name-for-id "shared-id") "Folio entry"))))

(ert-deftest folio--store-entry-with-name/failed-rename-keeps-old-bookmark ()
  "A failed replacement write does not delete the old bookmark."
  (let ((entry `((id . "id") (type . "url") (title . "New")
                 (url . "https://example.com")
                 (status . ,folio--status-unread)))
        deleted)
    (cl-letf (((symbol-function 'folio--ensure-bookmarks-loaded) #'ignore)
              ((symbol-function 'bookmark-get-bookmark) #'ignore)
              ((symbol-function 'bookmark-store)
               (lambda (&rest _args)
                 (error "simulated write failure")))
              ((symbol-function 'bookmark-delete)
               (lambda (name &optional _batch)
                 (setq deleted name))))
      (should-error (folio--store-entry-with-name entry "New" "Old"))
      (should-not deleted))))

(ert-deftest folio-add-url/rejects-empty-url-before-capture ()
  "Capturing an empty URL fails without storing a bookmark."
  (cl-letf (((symbol-function 'folio--ensure-bookmarks-loaded) #'ignore)
            ((symbol-function 'folio--capture-entry)
             (lambda (&rest _args)
               (ert-fail "capture should not run"))))
    (should-error (folio-add-url "   ") :type 'user-error)))

;;; ─── Delete selection ───────────────────────────────────────────────────────

(defun folio-test--set-list-ids (ids)
  "Populate the current tabulated list buffer with IDS."
  (setq tabulated-list-format [("Title" 20 t)]
        tabulated-list-entries
        (mapcar (lambda (id) (list id (vector id))) ids))
  (tabulated-list-init-header)
  (tabulated-list-print t))

(ert-deftest folio-list-delete/selects-following-entry ()
  "Deleting a middle entry keeps point on its row's following neighbor."
  (with-temp-buffer
    (tabulated-list-mode)
    (folio-test--set-list-ids '("alpha" "beta" "gamma"))
    (folio-list--goto-id "beta")
    (let ((fallback (folio-list--nearest-surviving-id '("beta"))))
      (cl-letf (((symbol-function 'folio-list-refresh)
                 (lambda ()
                   (folio-test--set-list-ids '("alpha" "gamma")))))
        (folio--refresh-keep-position fallback))
      (should (equal (tabulated-list-get-id) "gamma")))))

(ert-deftest folio-list-delete/selects-previous-after-last-entry ()
  "Deleting the final entry moves point to the preceding neighbor."
  (with-temp-buffer
    (tabulated-list-mode)
    (folio-test--set-list-ids '("alpha" "beta" "gamma"))
    (folio-list--goto-id "gamma")
    (let ((fallback (folio-list--nearest-surviving-id '("gamma"))))
      (cl-letf (((symbol-function 'folio-list-refresh)
                 (lambda ()
                   (folio-test--set-list-ids '("alpha" "beta")))))
        (folio--refresh-keep-position fallback))
      (should (equal (tabulated-list-get-id) "beta")))))

;;; folio-test.el ends here
