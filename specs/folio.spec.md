---
summary: Bookmark enhancement layer on Emacs bookmark system — URL/file entries with tags, read status, tabulated-list UI, tag filtering, marks + bulk delete/tag, nerd-icon support, and advice-based bookmark integration
---

# Folio

## Purpose

Thin enhancement layer on Emacs's built-in bookmark system. Adds URL bookmarks, a tabulated-list UI, per-entry metadata (tags, notes, read status, timestamps), and filtering/sorting.

## Entry Points

### Interactive Commands

| Command | Description |
|---------|-------------|
| `folio-list` | Open/create the `*Folio*` list buffer |
| `folio-add-url` | Add a URL bookmark (heuristic title from URL, prompts for tags/note) |
| `folio-add-file` | Add a file bookmark |
| `folio-bookmark-set` | Create a bookmark at point and eagerly adopt it into Folio |

### List Buffer Commands

| Key | Command | Description |
|-----|---------|-------------|
| `RET`/`o` | `folio-list-open` | Open entry at point |
| `/` | `folio-list-filter-tags` | Filter by tags (intersection) |
| `*` | `folio-list-toggle-read` | Toggle unread/read |
| `r` | `folio-list-edit-title` | Rename entry |
| `t` | `folio-list-edit-tags` | Edit tags on marked entries, or entry at point |
| `l` | `folio-list-edit-location` | Edit URL/path |
| `+` | `folio-list-edit-note` | Open note editor |
| `d` | `folio-list-delete` | Delete marked entries, or entry at point |
| `m` | `folio-list-mark` | Toggle mark at point; with region, mark all in region |
| `u` | `folio-list-unmark` | Unmark entry at point |
| `U` | `folio-list-unmark-all` | Clear all marks |
| `; l` | `folio-list-sort-by-title` | Sort alphabetically |
| `; t` | `folio-list-sort-by-time` | Sort by date (newest first) |
| `-` | `folio-add-url` | Add URL from list |
| `=` | `folio-add-file` | Add file from list |
| `g` | `folio-list-refresh` | Refresh display |

## Data Model

Entries are alists stored as Emacs bookmark records with folio-prefixed metadata keys.

### Entry Fields

| Key | Type | Description |
|-----|------|-------------|
| `id` | string | Unique ID; equals `folio-id` when present, else the bookmark name |
| `type` | string | `"url"`, `"file"`, or `"bookmark"` |
| `title` | string | Display name |
| `url` | string | Web URL (URL entries only) |
| `path` | string | File path (file entries only) |
| `tags` | list of strings | Cleaned, deduplicated, sorted |
| `note` | string | Annotation text |
| `status` | string | `"unread"` or `"read"` |
| `added` | string | Timestamp `"%Y-%m-%d %H:%M"` |
| `handler` | function | Bookmark handler |

### Bookmark Record Storage

Folio metadata is stored in bookmark records under keys: `folio-id`, `folio-tags`, `folio-status`, `folio-added`. Notes use the standard `annotation` key.

## Key Flows

### URL Capture
1. `folio-add-url` reads URL (default from thing-at-point or kill-ring)
2. `folio--normalize-url` adds `https://` if no scheme
3. `folio--guess-title-from-url` derives a title from the host/path heuristically (no network)
4. User confirms title, enters tags and optional note
5. `folio--store-entry-as-bookmark` generates a unique bookmark name, converts entry to record, calls `bookmark-store`
6. Cache invalidated via advice, list refreshed

### Lazy Adoption
Folio does not migrate bookmarks up front. Non-folio bookmarks render correctly in the list via fallbacks in `folio--bookmark-record->entry`:
- `id` falls back to the bookmark name
- `added` falls back to `last-modified` via `bookmark-time-to-time`
- `status` defaults to `"unread"`
- `tags`/`note` default to nil

A bookmark is promoted to "folio-owned" (gets `folio-id`, `folio-status`, `folio-added` persisted) on the first folio interaction:
- `folio-add-url` / `folio-add-file` create it with folio fields already set
- `folio-bookmark-set` eagerly adopts an existing bookmark
- `folio-list-edit-*` / `folio-list-toggle-read` write folio fields as part of the update
- `folio--bookmark-mark-read` (the `bookmark-jump` advice) adopts a bookmark on first jump and marks it read

### Read-Marking on Jump
1. `bookmark-jump` advice (`folio--bookmark-jump-advice`) wraps the call
2. Fringe mark suppressed for external (non-file, non-buffer) bookmarks
3. After jump, `folio--bookmark-mark-read` assigns folio metadata if missing, then sets status to `"read"` unless already read
4. List buffer refreshed if open

### Cache Lifecycle
- `folio--list-entries` is the single cache variable. The private
  `folio--cold-cache` sentinel means cold, so an empty database can be cached.
- `folio--entries` rebuilds from `bookmark-alist` when cold, returns the cache otherwise.
- `folio--refresh-db` invalidates and rebuilds. Used by `folio-list-refresh`.
- Advice on `bookmark-store`, `bookmark-delete`, `bookmark-load` calls `folio--invalidate-cache`.

### List Display
1. `folio-list-refresh` calls `folio--refresh-db` to guarantee a fresh view
2. Entries filtered by `folio--filter-tags` (all specified tags must match)
3. Sorted by `folio--entry<`: by `folio-list-sort-key` (title or added)
4. Converted to rows with `folio--entry->row` (faces, icons, clickable tags/links)
5. `folio-list--apply-marks` re-renders mark overlays so they survive the refresh

### Marks and Bulk Operations
Marks live in two buffer-local hash tables initialized by `folio-list-mode`:
`folio-list--marked` (id → t) and `folio-list--mark-overlays`
(id → (row-overlay . indicator-overlay)).

- `folio-list-mark` toggles the mark at point (or marks every row in an
  active region, no toggle) and advances.
- `folio-list-unmark` / `folio-list-unmark-all` remove marks.
- `folio-list-delete` acts on marked entries if any are marked, otherwise on
  the entry at point. Plain bookmarks without a `folio-id` use their bookmark
  name as the fallback ID and can be deleted before adoption. After deletion,
  point moves to the nearest surviving row, preferring the following row when
  two candidates are equally near.
- `folio-list-edit-tags` acts on marked entries if any are marked: prompts
  for a tag list and **replaces** the tags on each marked entry after
  confirmation. Without marks it edits the entry at point, seeded with its
  current tags (same replacement semantic, single target).
- Mark overlays are two overlays per row: one with `folio-list-mark-face`
  covering the line, and one single-character overlay whose `display`
  property renders a `*` in `folio-list-mark-indicator-face`.

### Record Merging
`folio--merge-record-allow-remove` deep-merges updated fields into existing bookmark records. Keys in the allow-remove list (`annotation`, `folio-tags`) are deleted when value is nil rather than set to nil — this prevents stale empty keys.

## Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `folio-url-open-function` | `browse-url` | How to open URLs |
| `folio-list-sort-key` | `'added` | Sort by `'added` or `'title` |

The Title column is prefixed with a type indicator: a Nerd Font icon when the optional `nerd-icons` package is available, or a single uppercase letter (`U`rl / `F`ile / `B`ookmark) otherwise.

## Faces

`folio-title-face`, `folio-unread-face`, `folio-type-url-face`, `folio-type-file-face`, `folio-tags-face`, `folio-location-face`, `folio-note-face`, `folio-timestamp-face`, `folio-list-mark-face`, `folio-list-mark-indicator-face`

## Dependencies

- Emacs built-ins: `bookmark`, `cl-lib`, `url-parse`, `seq`, `subr-x`, `tabulated-list`, `thingatpt`
- Optional: `nerd-icons`

## Integration

- Advises `bookmark-jump`, `bookmark-store`, `bookmark-delete`, `bookmark-load`
- Custom handler `folio-bookmark-url-handler` for URL bookmarks
- Clean unload via `folio-unload-function` (removes all advice)

## Edge Cases

- URL title is derived purely from the URL string (host + last path segment). No network calls.
- Empty URLs are rejected during capture and location editing.
- Bookmark name conflicts resolved by appending `(2)`, `(3)`, etc.
- Existing bookmark files may contain `folio-status` values written by older versions (e.g., `"archived"`). These load correctly but display as neither unread nor read; toggling promotes them to a normal status.
